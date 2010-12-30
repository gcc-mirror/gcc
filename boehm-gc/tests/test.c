/* 
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996 by Silicon Graphics.  All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */
/* An incomplete test for the garbage collector.  		*/
/* Some more obscure entry points are not tested at all.	*/
/* This must be compiled with the same flags used to build the 	*/
/* GC.  It uses GC internals to allow more precise results	*/
/* checking for some of the tests.				*/

# undef GC_BUILD

#if defined(DBG_HDRS_ALL) || defined(MAKE_BACK_GRAPH)
#  define GC_DEBUG
#endif

# if defined(mips) && defined(SYSTYPE_BSD43)
    /* MIPS RISCOS 4 */
# else
#   include <stdlib.h>
# endif
# include <stdio.h>
# ifdef _WIN32_WCE
#   include <winbase.h>
#   define assert ASSERT
# else
#   include <assert.h>        /* Not normally used, but handy for debugging. */
# endif
# include <assert.h>	/* Not normally used, but handy for debugging. */
# include "gc.h"
# include "gc_typed.h"
# ifdef THREAD_LOCAL_ALLOC
#   include "gc_local_alloc.h"
# endif
# include "private/gc_priv.h"	/* For output, locking, MIN_WORDS, 	*/
				/* and some statistics.			*/
# include "private/gcconfig.h"

# if defined(MSWIN32) || defined(MSWINCE)
#   include <windows.h>
# endif

# ifdef PCR
#   include "th/PCR_ThCrSec.h"
#   include "th/PCR_Th.h"
#   undef GC_printf0
#   define GC_printf0 printf
#   undef GC_printf1
#   define GC_printf1 printf
# endif

# if defined(GC_SOLARIS_THREADS) && !defined(GC_SOLARIS_PTHREADS)
#   include <thread.h>
#   include <synch.h>
# endif

# if defined(GC_PTHREADS)
#   include <pthread.h>
# endif

# if defined(GC_WIN32_THREADS) && !defined(GC_PTHREADS)
    static CRITICAL_SECTION incr_cs;
# endif

#ifdef __STDC__
# include <stdarg.h>
#endif


/* Allocation Statistics */
int stubborn_count = 0;
int uncollectable_count = 0;
int collectable_count = 0;
int atomic_count = 0;
int realloc_count = 0;

#if defined(GC_AMIGA_FASTALLOC) && defined(AMIGA)

  extern void GC_amiga_free_all_mem(void);
  void Amiga_Fail(void){GC_amiga_free_all_mem();abort();}
# define FAIL (void)Amiga_Fail()
  void *GC_amiga_gctest_malloc_explicitly_typed(size_t lb, GC_descr d){
    void *ret=GC_malloc_explicitly_typed(lb,d);
    if(ret==NULL){
		if(!GC_dont_gc){
	      GC_gcollect();
	      ret=GC_malloc_explicitly_typed(lb,d);
		}
      if(ret==NULL){
        GC_printf0("Out of memory, (typed allocations are not directly "
		   "supported with the GC_AMIGA_FASTALLOC option.)\n");
        FAIL;
      }
    }
    return ret;
  }
  void *GC_amiga_gctest_calloc_explicitly_typed(size_t a,size_t lb, GC_descr d){
    void *ret=GC_calloc_explicitly_typed(a,lb,d);
    if(ret==NULL){
		if(!GC_dont_gc){
	      GC_gcollect();
	      ret=GC_calloc_explicitly_typed(a,lb,d);
		}
      if(ret==NULL){
        GC_printf0("Out of memory, (typed allocations are not directly "
		   "supported with the GC_AMIGA_FASTALLOC option.)\n");
        FAIL;
      }
    }
    return ret;
  }
# define GC_malloc_explicitly_typed(a,b) GC_amiga_gctest_malloc_explicitly_typed(a,b) 
# define GC_calloc_explicitly_typed(a,b,c) GC_amiga_gctest_calloc_explicitly_typed(a,b,c) 

#else /* !AMIGA_FASTALLOC */

# ifdef PCR
#   define FAIL (void)abort()
# else
#   ifdef MSWINCE
#     define FAIL DebugBreak()
#   else
#     define FAIL GC_abort("Test failed");
#   endif
# endif

#endif /* !AMIGA_FASTALLOC */

/* AT_END may be defined to exercise the interior pointer test	*/
/* if the collector is configured with ALL_INTERIOR_POINTERS.   */
/* As it stands, this test should succeed with either		*/
/* configuration.  In the FIND_LEAK configuration, it should	*/
/* find lots of leaks, since we free almost nothing.		*/

struct SEXPR {
    struct SEXPR * sexpr_car;
    struct SEXPR * sexpr_cdr;
};


typedef struct SEXPR * sexpr;

# define INT_TO_SEXPR(x) ((sexpr)(unsigned long)(x))

# undef nil
# define nil (INT_TO_SEXPR(0))
# define car(x) ((x) -> sexpr_car)
# define cdr(x) ((x) -> sexpr_cdr)
# define is_nil(x) ((x) == nil)


int extra_count = 0;        /* Amount of space wasted in cons node */

/* Silly implementation of Lisp cons. Intentionally wastes lots of space */
/* to test collector.                                                    */
# ifdef VERY_SMALL_CONFIG
#   define cons small_cons
# else
sexpr cons (x, y)
sexpr x;
sexpr y;
{
    register sexpr r;
    register int *p;
    register int my_extra = extra_count;
    
    stubborn_count++;
    r = (sexpr) GC_MALLOC_STUBBORN(sizeof(struct SEXPR) + my_extra);
    if (r == 0) {
        (void)GC_printf0("Out of memory\n");
        exit(1);
    }
    for (p = (int *)r;
         ((char *)p) < ((char *)r) + my_extra + sizeof(struct SEXPR); p++) {
	if (*p) {
	    (void)GC_printf1("Found nonzero at 0x%lx - allocator is broken\n",
	    		     (unsigned long)p);
	    FAIL;
        }
        *p = 13;
    }
#   ifdef AT_END
	r = (sexpr)((char *)r + (my_extra & ~7));
#   endif
    r -> sexpr_car = x;
    r -> sexpr_cdr = y;
    my_extra++;
    if ( my_extra >= 5000 ) {
        extra_count = 0;
    } else {
        extra_count = my_extra;
    }
    GC_END_STUBBORN_CHANGE((char *)r);
    return(r);
}
# endif

#ifdef GC_GCJ_SUPPORT

#include "gc_mark.h"
#include "gc_gcj.h"

/* The following struct emulates the vtable in gcj.	*/
/* This assumes the default value of MARK_DESCR_OFFSET. */
struct fake_vtable {
  void * dummy;		/* class pointer in real gcj.	*/
  size_t descr;
};

struct fake_vtable gcj_class_struct1 = { 0, sizeof(struct SEXPR)
					    + sizeof(struct fake_vtable *) };
			/* length based descriptor.	*/
struct fake_vtable gcj_class_struct2 =
				{ 0, (3l << (CPP_WORDSZ - 3)) | GC_DS_BITMAP};
			/* Bitmap based descriptor.	*/

struct GC_ms_entry * fake_gcj_mark_proc(word * addr,
				        struct GC_ms_entry *mark_stack_ptr,
				        struct GC_ms_entry *mark_stack_limit,
				        word env   )
{
    sexpr x;
    if (1 == env) {
	/* Object allocated with debug allocator.	*/
	addr = (word *)GC_USR_PTR_FROM_BASE(addr);
    }
    x = (sexpr)(addr + 1); /* Skip the vtable pointer. */
    mark_stack_ptr = GC_MARK_AND_PUSH(
			      (GC_PTR)(x -> sexpr_cdr), mark_stack_ptr,
			      mark_stack_limit, (GC_PTR *)&(x -> sexpr_cdr));
    mark_stack_ptr = GC_MARK_AND_PUSH(
			      (GC_PTR)(x -> sexpr_car), mark_stack_ptr,
			      mark_stack_limit, (GC_PTR *)&(x -> sexpr_car));
    return(mark_stack_ptr);
}

#endif /* GC_GCJ_SUPPORT */

#ifdef THREAD_LOCAL_ALLOC

#undef GC_REDIRECT_TO_LOCAL
#include "gc_local_alloc.h"

sexpr local_cons (x, y)
sexpr x;
sexpr y;
{
    register sexpr r;
    register int *p;
    register int my_extra = extra_count;
    static int my_random = 0;
    
    collectable_count++;
    r = (sexpr) GC_LOCAL_MALLOC(sizeof(struct SEXPR) + my_extra);
#   ifdef GC_GCJ_SUPPORT
      if (collectable_count % 2 == 0) {
        r = (sexpr) GC_LOCAL_GCJ_MALLOC(sizeof(struct SEXPR) + sizeof(GC_word) + my_extra,
					&gcj_class_struct1);
        r = (sexpr) ((GC_word *)r + 1);
      }
#   endif
    if (r == 0) {
        (void)GC_printf0("Out of memory\n");
        exit(1);
    }
    for (p = (int *)r;
         ((char *)p) < ((char *)r) + my_extra + sizeof(struct SEXPR); p++) {
	if (*p) {
	    (void)GC_printf1("Found nonzero at 0x%lx (local) - allocator is broken\n",
	    		     (unsigned long)p);
	    FAIL;
        }
        *p = 13;
    }
    r -> sexpr_car = x;
    r -> sexpr_cdr = y;
    my_extra++;
    if ( my_extra >= 5000 || my_extra == 200 && ++my_random % 37 != 0) {
        extra_count = 0;
    } else {
        extra_count = my_extra;
    }
    return(r);
}
#endif /* THREAD_LOCAL_ALLOC */

sexpr small_cons (x, y)
sexpr x;
sexpr y;
{
    register sexpr r;
    
    collectable_count++;
    r = (sexpr) GC_MALLOC(sizeof(struct SEXPR));
    if (r == 0) {
        (void)GC_printf0("Out of memory\n");
        exit(1);
    }
    r -> sexpr_car = x;
    r -> sexpr_cdr = y;
    return(r);
}

sexpr small_cons_uncollectable (x, y)
sexpr x;
sexpr y;
{
    register sexpr r;
    
    uncollectable_count++;
    r = (sexpr) GC_MALLOC_UNCOLLECTABLE(sizeof(struct SEXPR));
    if (r == 0) {
        (void)GC_printf0("Out of memory\n");
        exit(1);
    }
    r -> sexpr_car = x;
    r -> sexpr_cdr = (sexpr)(~(unsigned long)y);
    return(r);
}

#ifdef GC_GCJ_SUPPORT


sexpr gcj_cons(x, y)
sexpr x;
sexpr y;
{
    GC_word * r;
    sexpr result;
    static int count = 0;
    
    if (++count & 1) {
#     ifdef USE_MARK_BYTES
        r = (GC_word *) GC_GCJ_FAST_MALLOC(4, &gcj_class_struct1);
#     else
        r = (GC_word *) GC_GCJ_FAST_MALLOC(3, &gcj_class_struct1);
#     endif
    } else {
        r = (GC_word *) GC_GCJ_MALLOC(sizeof(struct SEXPR)
				      + sizeof(struct fake_vtable*),
				      &gcj_class_struct2);
    }
    if (r == 0) {
        (void)GC_printf0("Out of memory\n");
        exit(1);
    }
    result = (sexpr)(r + 1);
    result -> sexpr_car = x;
    result -> sexpr_cdr = y;
    return(result);
}
#endif

/* Return reverse(x) concatenated with y */
sexpr reverse1(x, y)
sexpr x, y;
{
    if (is_nil(x)) {
        return(y);
    } else {
        return( reverse1(cdr(x), cons(car(x), y)) );
    }
}

sexpr reverse(x)
sexpr x;
{
#   ifdef TEST_WITH_SYSTEM_MALLOC
      malloc(100000);
#   endif
    return( reverse1(x, nil) );
}

sexpr ints(low, up)
int low, up;
{
    if (low > up) {
	return(nil);
    } else {
        return(small_cons(small_cons(INT_TO_SEXPR(low), nil), ints(low+1, up)));
    }
}

#ifdef GC_GCJ_SUPPORT
/* Return reverse(x) concatenated with y */
sexpr gcj_reverse1(x, y)
sexpr x, y;
{
    if (is_nil(x)) {
        return(y);
    } else {
        return( gcj_reverse1(cdr(x), gcj_cons(car(x), y)) );
    }
}

sexpr gcj_reverse(x)
sexpr x;
{
    return( gcj_reverse1(x, nil) );
}

sexpr gcj_ints(low, up)
int low, up;
{
    if (low > up) {
	return(nil);
    } else {
        return(gcj_cons(gcj_cons(INT_TO_SEXPR(low), nil), gcj_ints(low+1, up)));
    }
}
#endif /* GC_GCJ_SUPPORT */

#ifdef THREAD_LOCAL_ALLOC
/* Return reverse(x) concatenated with y */
sexpr local_reverse1(x, y)
sexpr x, y;
{
    if (is_nil(x)) {
        return(y);
    } else {
        return( local_reverse1(cdr(x), local_cons(car(x), y)) );
    }
}

sexpr local_reverse(x)
sexpr x;
{
    return( local_reverse1(x, nil) );
}

sexpr local_ints(low, up)
int low, up;
{
    if (low > up) {
	return(nil);
    } else {
        return(local_cons(local_cons(INT_TO_SEXPR(low), nil), local_ints(low+1, up)));
    }
}
#endif /* THREAD_LOCAL_ALLOC */

/* To check uncollectable allocation we build lists with disguised cdr	*/
/* pointers, and make sure they don't go away.				*/
sexpr uncollectable_ints(low, up)
int low, up;
{
    if (low > up) {
	return(nil);
    } else {
        return(small_cons_uncollectable(small_cons(INT_TO_SEXPR(low), nil),
               uncollectable_ints(low+1, up)));
    }
}

void check_ints(list, low, up)
sexpr list;
int low, up;
{
    if ((int)(GC_word)(car(car(list))) != low) {
        (void)GC_printf0(
           "List reversal produced incorrect list - collector is broken\n");
        FAIL;
    }
    if (low == up) {
        if (cdr(list) != nil) {
           (void)GC_printf0("List too long - collector is broken\n");
           FAIL;
        }
    } else {
        check_ints(cdr(list), low+1, up);
    }
}

# define UNCOLLECTABLE_CDR(x) (sexpr)(~(unsigned long)(cdr(x)))

void check_uncollectable_ints(list, low, up)
sexpr list;
int low, up;
{
    if ((int)(GC_word)(car(car(list))) != low) {
        (void)GC_printf0(
           "Uncollectable list corrupted - collector is broken\n");
        FAIL;
    }
    if (low == up) {
        if (UNCOLLECTABLE_CDR(list) != nil) {
           (void)GC_printf0("Uncollectable list too long - collector is broken\n");
           FAIL;
        }
    } else {
        check_uncollectable_ints(UNCOLLECTABLE_CDR(list), low+1, up);
    }
}

/* Not used, but useful for debugging: */
void print_int_list(x)
sexpr x;
{
    if (is_nil(x)) {
        (void)GC_printf0("NIL\n");
    } else {
        (void)GC_printf1("(%ld)", (long)(car(car(x))));
        if (!is_nil(cdr(x))) {
            (void)GC_printf0(", ");
            (void)print_int_list(cdr(x));
        } else {
            (void)GC_printf0("\n");
        }
    }
}

/*
 * A tiny list reversal test to check thread creation.
 */
#ifdef THREADS

# if defined(GC_WIN32_THREADS) && !defined(CYGWIN32)
    DWORD  __stdcall tiny_reverse_test(void * arg)
# else
    void * tiny_reverse_test(void * arg)
# endif
{
    int i;
    for (i = 0; i < 5; ++i) {
      check_ints(reverse(reverse(ints(1,10))), 1, 10);
#     ifdef THREAD_LOCAL_ALLOC
        check_ints(local_reverse(local_reverse(local_ints(1,10))), 1, 10);
#     endif
    }
    return 0;
}

# if defined(GC_PTHREADS)
    void fork_a_thread()
    {
      pthread_t t;
      int code;
      if ((code = pthread_create(&t, 0, tiny_reverse_test, 0)) != 0) {
    	(void)GC_printf1("Small thread creation failed %lu\n",
		         (unsigned long)code);
    	FAIL;
      }
      if ((code = pthread_join(t, 0)) != 0) {
        (void)GC_printf1("Small thread join failed %lu\n",
	(unsigned long)code);
        FAIL;
      }
    }

# elif defined(GC_WIN32_THREADS)
    void fork_a_thread()
    {
  	DWORD thread_id;
	HANDLE h;
    	h = GC_CreateThread(NULL, 0, tiny_reverse_test, 0, 0, &thread_id);
        if (h == (HANDLE)NULL) {
            (void)GC_printf1("Small thread creation failed %lu\n",
			     (unsigned long)GetLastError());
      	    FAIL;
        }
    	if (WaitForSingleObject(h, INFINITE) != WAIT_OBJECT_0) {
      	    (void)GC_printf1("Small thread wait failed %lu\n",
			     (unsigned long)GetLastError());
      	    FAIL;
    	}
    }

/* # elif defined(GC_SOLARIS_THREADS) */

# else

#   define fork_a_thread()

# endif

#else

# define fork_a_thread()

#endif 

/* Try to force a to be strangely aligned */
struct {
  char dummy;
  sexpr aa;
} A;
#define a A.aa

/*
 * Repeatedly reverse lists built out of very different sized cons cells.
 * Check that we didn't lose anything.
 */
void reverse_test()
{
    int i;
    sexpr b;
    sexpr c;
    sexpr d;
    sexpr e;
    sexpr *f, *g, *h;
#   if defined(MSWIN32) || defined(MACOS)
      /* Win32S only allows 128K stacks */
#     define BIG 1000
#   else
#     if defined PCR
	/* PCR default stack is 100K.  Stack frames are up to 120 bytes. */
#	define BIG 700
#     else
#	if defined MSWINCE
	  /* WinCE only allows 64K stacks */
#	  define BIG 500
#	else
#	  if defined(OSF1)
	    /* OSF has limited stack space by default, and large frames. */
#           define BIG 200
#	  else
#	    if defined(__MACH__) && defined(__ppc64__)
	      /* Small stack and largish frames.  */
#             define BIG 2500	      
#	    else
#             define BIG 4500
#	    endif
#	  endif
#	endif
#     endif
#   endif

    A.dummy = 17;
    a = ints(1, 49);
    b = ints(1, 50);
    c = ints(1, BIG);
    d = uncollectable_ints(1, 100);
    e = uncollectable_ints(1, 1);
    /* Check that realloc updates object descriptors correctly */
    collectable_count++;
    f = (sexpr *)GC_MALLOC(4 * sizeof(sexpr));
    realloc_count++;
    f = (sexpr *)GC_REALLOC((GC_PTR)f, 6 * sizeof(sexpr));
    f[5] = ints(1,17);
    collectable_count++;
    g = (sexpr *)GC_MALLOC(513 * sizeof(sexpr));
    realloc_count++;
    g = (sexpr *)GC_REALLOC((GC_PTR)g, 800 * sizeof(sexpr));
    g[799] = ints(1,18);
    collectable_count++;
    h = (sexpr *)GC_MALLOC(1025 * sizeof(sexpr));
    realloc_count++;
    h = (sexpr *)GC_REALLOC((GC_PTR)h, 2000 * sizeof(sexpr));
#   ifdef GC_GCJ_SUPPORT
      h[1999] = gcj_ints(1,200);
      for (i = 0; i < 51; ++i) 
        h[1999] = gcj_reverse(h[1999]);
      /* Leave it as the reveresed list for now. */
#   else
      h[1999] = ints(1,200);
#   endif
    /* Try to force some collections and reuse of small list elements */
      for (i = 0; i < 10; i++) {
        (void)ints(1, BIG);
      }
    /* Superficially test interior pointer recognition on stack */
      c = (sexpr)((char *)c + sizeof(char *));
      d = (sexpr)((char *)d + sizeof(char *));

#   ifdef __STDC__
        GC_FREE((void *)e);
#   else
        GC_FREE((char *)e);
#   endif
    check_ints(b,1,50);
    check_ints(a,1,49);
    for (i = 0; i < 50; i++) {
        check_ints(b,1,50);
        b = reverse(reverse(b));
    }
    check_ints(b,1,50);
    check_ints(a,1,49);
    for (i = 0; i < 60; i++) {
	if (i % 10 == 0) fork_a_thread();
    	/* This maintains the invariant that a always points to a list of */
    	/* 49 integers.  Thus this is thread safe without locks,	  */
    	/* assuming atomic pointer assignments.				  */
        a = reverse(reverse(a));
#       ifdef THREAD_LOCAL_ALLOC
	  a = local_reverse(local_reverse(a));
#	endif
#	if !defined(AT_END) && !defined(THREADS)
	  /* This is not thread safe, since realloc explicitly deallocates */
          if (i & 1) {
            a = (sexpr)GC_REALLOC((GC_PTR)a, 500);
          } else {
            a = (sexpr)GC_REALLOC((GC_PTR)a, 8200);
          }
#	endif
    }
    check_ints(a,1,49);
    check_ints(b,1,50);
    c = (sexpr)((char *)c - sizeof(char *));
    d = (sexpr)((char *)d - sizeof(char *));
    check_ints(c,1,BIG);
    check_uncollectable_ints(d, 1, 100);
    check_ints(f[5], 1,17);
    check_ints(g[799], 1,18);
#   ifdef GC_GCJ_SUPPORT
      h[1999] = gcj_reverse(h[1999]);
#   endif
    check_ints(h[1999], 1,200);
#   ifndef THREADS
	a = 0;
#   endif  
    b = c = 0;
}

#undef a

/*
 * The rest of this builds balanced binary trees, checks that they don't
 * disappear, and tests finalization.
 */
typedef struct treenode {
    int level;
    struct treenode * lchild;
    struct treenode * rchild;
} tn;

int finalizable_count = 0;
int finalized_count = 0;
VOLATILE int dropped_something = 0;

# ifdef __STDC__
  void finalizer(void * obj, void * client_data)
# else
  void finalizer(obj, client_data)
  char * obj;
  char * client_data;
# endif
{
  tn * t = (tn *)obj;

# ifdef PCR
     PCR_ThCrSec_EnterSys();
# endif
# if defined(GC_SOLARIS_THREADS) && !defined(GC_SOLARIS_PTHREADS)
    static mutex_t incr_lock;
    mutex_lock(&incr_lock);
# endif
# if  defined(GC_PTHREADS)
    static pthread_mutex_t incr_lock = PTHREAD_MUTEX_INITIALIZER;
    pthread_mutex_lock(&incr_lock);
# else
#   ifdef GC_WIN32_THREADS
      EnterCriticalSection(&incr_cs);
#   endif
# endif
  if ((int)(GC_word)client_data != t -> level) {
     (void)GC_printf0("Wrong finalization data - collector is broken\n");
     FAIL;
  }
  finalized_count++;
  t -> level = -1;	/* detect duplicate finalization immediately */
# ifdef PCR
    PCR_ThCrSec_ExitSys();
# endif
# if defined(GC_SOLARIS_THREADS) && !defined(GC_SOLARIS_PTHREADS)
    mutex_unlock(&incr_lock);
# endif
# if defined(GC_PTHREADS)
    pthread_mutex_unlock(&incr_lock);
# else
#   ifdef GC_WIN32_THREADS
      LeaveCriticalSection(&incr_cs);
#   endif
# endif
}

size_t counter = 0;

# define MAX_FINALIZED 8000

# if !defined(MACOS)
  GC_FAR GC_word live_indicators[MAX_FINALIZED] = {0};
#else
  /* Too big for THINK_C. have to allocate it dynamically. */
  GC_word *live_indicators = 0;
#endif

int live_indicators_count = 0;

tn * mktree(n)
int n;
{
#   ifdef THREAD_LOCAL_ALLOC
      tn * result = (tn *)GC_LOCAL_MALLOC(sizeof(tn));
#   else
      tn * result = (tn *)GC_MALLOC(sizeof(tn));
#   endif
    
    collectable_count++;
#   ifdef THREAD_LOCAL_ALLOC
       /* Minimally exercise thread local allocation */
       {
         char * result = (char *)GC_LOCAL_MALLOC_ATOMIC(17);
	 memset(result, 'a', 17);
       }
#   endif /* THREAD_LOCAL_ALLOC */
#   if defined(MACOS)
	/* get around static data limitations. */
	if (!live_indicators)
		live_indicators =
		    (GC_word*)NewPtrClear(MAX_FINALIZED * sizeof(GC_word));
	if (!live_indicators) {
          (void)GC_printf0("Out of memory\n");
          exit(1);
        }
#   endif
    if (n == 0) return(0);
    if (result == 0) {
        (void)GC_printf0("Out of memory\n");
        exit(1);
    }
    result -> level = n;
    result -> lchild = mktree(n-1);
    result -> rchild = mktree(n-1);
    if (counter++ % 17 == 0 && n >= 2) {
        tn * tmp = result -> lchild -> rchild;
        
        result -> lchild -> rchild = result -> rchild -> lchild;
        result -> rchild -> lchild = tmp;
    }
    if (counter++ % 119 == 0) {
        int my_index;
        
        {
#	  ifdef PCR
 	    PCR_ThCrSec_EnterSys();
#	  endif
#	  if defined(GC_SOLARIS_THREADS) && !defined(GC_SOLARIS_PTHREADS)
	    static mutex_t incr_lock;
	    mutex_lock(&incr_lock);
#	  endif
#         if defined(GC_PTHREADS)
            static pthread_mutex_t incr_lock = PTHREAD_MUTEX_INITIALIZER;
            pthread_mutex_lock(&incr_lock);
#         else
#           ifdef GC_WIN32_THREADS
              EnterCriticalSection(&incr_cs);
#           endif
#         endif
		/* Losing a count here causes erroneous report of failure. */
          finalizable_count++;
          my_index = live_indicators_count++;
#	  ifdef PCR
 	    PCR_ThCrSec_ExitSys();
#	  endif
#	  if defined(GC_SOLARIS_THREADS) && !defined(GC_SOLARIS_PTHREADS)
	    mutex_unlock(&incr_lock);
#	  endif
#	  if defined(GC_PTHREADS)
	    pthread_mutex_unlock(&incr_lock);
#	  else
#           ifdef GC_WIN32_THREADS
              LeaveCriticalSection(&incr_cs);
#           endif
#         endif
	}

        GC_REGISTER_FINALIZER((GC_PTR)result, finalizer, (GC_PTR)(GC_word)n,
        		      (GC_finalization_proc *)0, (GC_PTR *)0);
        if (my_index >= MAX_FINALIZED) {
		GC_printf0("live_indicators overflowed\n");
		FAIL;
	}
        live_indicators[my_index] = 13;
        if (GC_GENERAL_REGISTER_DISAPPEARING_LINK(
         	(GC_PTR *)(&(live_indicators[my_index])),
         	(GC_PTR)result) != 0) {
         	GC_printf0("GC_general_register_disappearing_link failed\n");
         	FAIL;
        }
        if (GC_unregister_disappearing_link(
         	(GC_PTR *)
         	   (&(live_indicators[my_index]))) == 0) {
         	GC_printf0("GC_unregister_disappearing_link failed\n");
         	FAIL;
        }
        if (GC_GENERAL_REGISTER_DISAPPEARING_LINK(
         	(GC_PTR *)(&(live_indicators[my_index])),
         	(GC_PTR)result) != 0) {
         	GC_printf0("GC_general_register_disappearing_link failed 2\n");
         	FAIL;
        }
    }
    return(result);
}

void chktree(t,n)
tn *t;
int n;
{
    if (n == 0 && t != 0) {
        (void)GC_printf0("Clobbered a leaf - collector is broken\n");
        FAIL;
    }
    if (n == 0) return;
    if (t -> level != n) {
        (void)GC_printf1("Lost a node at level %lu - collector is broken\n",
        		 (unsigned long)n);
        FAIL;
    }
    if (counter++ % 373 == 0) {
	collectable_count++;
	(void) GC_MALLOC(counter%5001);
    }
    chktree(t -> lchild, n-1);
    if (counter++ % 73 == 0) {
	collectable_count++;
	(void) GC_MALLOC(counter%373);
    }
    chktree(t -> rchild, n-1);
}

# if defined(GC_SOLARIS_THREADS) && !defined(GC_SOLARIS_PTHREADS)
thread_key_t fl_key;

void * alloc8bytes()
{
# if defined(SMALL_CONFIG) || defined(GC_DEBUG)
    collectable_count++;
    return(GC_MALLOC(8));
# else
    void ** my_free_list_ptr;
    void * my_free_list;
    
    if (thr_getspecific(fl_key, (void **)(&my_free_list_ptr)) != 0) {
    	(void)GC_printf0("thr_getspecific failed\n");
    	FAIL;
    }
    if (my_free_list_ptr == 0) {
        uncollectable_count++;
        my_free_list_ptr = GC_NEW_UNCOLLECTABLE(void *);
        if (thr_setspecific(fl_key, my_free_list_ptr) != 0) {
    	    (void)GC_printf0("thr_setspecific failed\n");
    	    FAIL;
        }
    }
    my_free_list = *my_free_list_ptr;
    if (my_free_list == 0) {
        collectable_count++;
        my_free_list = GC_malloc_many(8);
        if (my_free_list == 0) {
            (void)GC_printf0("alloc8bytes out of memory\n");
    	    FAIL;
        }
    }
    *my_free_list_ptr = GC_NEXT(my_free_list);
    GC_NEXT(my_free_list) = 0;
    return(my_free_list);
# endif
}

#else

# if defined(GC_PTHREADS)
pthread_key_t fl_key;

void * alloc8bytes()
{
# if defined(SMALL_CONFIG) || defined(GC_DEBUG)
    collectable_count++;
    return(GC_MALLOC(8));
# else
    void ** my_free_list_ptr;
    void * my_free_list;
    
    my_free_list_ptr = (void **)pthread_getspecific(fl_key);
    if (my_free_list_ptr == 0) {
        uncollectable_count++;
        my_free_list_ptr = GC_NEW_UNCOLLECTABLE(void *);
        if (pthread_setspecific(fl_key, my_free_list_ptr) != 0) {
    	    (void)GC_printf0("pthread_setspecific failed\n");
    	    FAIL;
        }
    }
    my_free_list = *my_free_list_ptr;
    if (my_free_list == 0) {
        my_free_list = GC_malloc_many(8);
        if (my_free_list == 0) {
            (void)GC_printf0("alloc8bytes out of memory\n");
    	    FAIL;
        }
    }
    *my_free_list_ptr = GC_NEXT(my_free_list);
    GC_NEXT(my_free_list) = 0;
    collectable_count++;
    return(my_free_list);
# endif
}

# else
#   define alloc8bytes() GC_MALLOC_ATOMIC(8)
# endif
#endif

void alloc_small(n)
int n;
{
    register int i;
    
    for (i = 0; i < n; i += 8) {
        atomic_count++;
        if (alloc8bytes() == 0) {
            (void)GC_printf0("Out of memory\n");
            FAIL;
        }
    }
}

# if defined(THREADS) && defined(GC_DEBUG)
#   ifdef VERY_SMALL_CONFIG
#     define TREE_HEIGHT 12
#   else
#     define TREE_HEIGHT 15
#   endif
# else
#   ifdef VERY_SMALL_CONFIG
#     define TREE_HEIGHT 13
#   else
#     define TREE_HEIGHT 16
#   endif
# endif
void tree_test()
{
    tn * root;
    register int i;
    
    root = mktree(TREE_HEIGHT);
#   ifndef VERY_SMALL_CONFIG
      alloc_small(5000000);
#   endif
    chktree(root, TREE_HEIGHT);
    if (finalized_count && ! dropped_something) {
        (void)GC_printf0("Premature finalization - collector is broken\n");
        FAIL;
    }
    dropped_something = 1;
    GC_noop(root);	/* Root needs to remain live until	*/
    			/* dropped_something is set.		*/
    root = mktree(TREE_HEIGHT);
    chktree(root, TREE_HEIGHT);
    for (i = TREE_HEIGHT; i >= 0; i--) {
        root = mktree(i);
        chktree(root, i);
    }
#   ifndef VERY_SMALL_CONFIG
      alloc_small(5000000);
#   endif
}

unsigned n_tests = 0;

GC_word bm_huge[10] = {
    0xffffffff,
    0xffffffff,
    0xffffffff,
    0xffffffff,
    0xffffffff,
    0xffffffff,
    0xffffffff,
    0xffffffff,
    0xffffffff,
    0x00ffffff,
};

/* A very simple test of explicitly typed allocation	*/
void typed_test()
{
    GC_word * old, * new;
    GC_word bm3 = 0x3;
    GC_word bm2 = 0x2;
    GC_word bm_large = 0xf7ff7fff;
    GC_descr d1 = GC_make_descriptor(&bm3, 2);
    GC_descr d2 = GC_make_descriptor(&bm2, 2);
#   ifndef LINT
      GC_descr dummy = GC_make_descriptor(&bm_large, 32);
#   endif
    GC_descr d3 = GC_make_descriptor(&bm_large, 32);
    GC_descr d4 = GC_make_descriptor(bm_huge, 320);
    GC_word * x = (GC_word *)GC_malloc_explicitly_typed(2000, d4);
    register int i;
    
    collectable_count++;
    old = 0;
    for (i = 0; i < 4000; i++) {
	collectable_count++;
        new = (GC_word *) GC_malloc_explicitly_typed(4 * sizeof(GC_word), d1);
        if (0 != new[0] || 0 != new[1]) {
	    GC_printf0("Bad initialization by GC_malloc_explicitly_typed\n");
	    FAIL;
	}
        new[0] = 17;
        new[1] = (GC_word)old;
        old = new;
	collectable_count++;
        new = (GC_word *) GC_malloc_explicitly_typed(4 * sizeof(GC_word), d2);
        new[0] = 17;
        new[1] = (GC_word)old;
        old = new;
	collectable_count++;
        new = (GC_word *) GC_malloc_explicitly_typed(33 * sizeof(GC_word), d3);
        new[0] = 17;
        new[1] = (GC_word)old;
        old = new;
	collectable_count++;
        new = (GC_word *) GC_calloc_explicitly_typed(4, 2 * sizeof(GC_word),
        					     d1);
        new[0] = 17;
        new[1] = (GC_word)old;
        old = new;
	collectable_count++;
        if (i & 0xff) {
          new = (GC_word *) GC_calloc_explicitly_typed(7, 3 * sizeof(GC_word),
        					     d2);
        } else {
          new = (GC_word *) GC_calloc_explicitly_typed(1001,
          					       3 * sizeof(GC_word),
        					       d2);
          if (0 != new[0] || 0 != new[1]) {
	    GC_printf0("Bad initialization by GC_malloc_explicitly_typed\n");
	    FAIL;
	  }
        }
        new[0] = 17;
        new[1] = (GC_word)old;
        old = new;
    }
    for (i = 0; i < 20000; i++) {
        if (new[0] != 17) {
            (void)GC_printf1("typed alloc failed at %lu\n",
            		     (unsigned long)i);
            FAIL;
        }
        new[0] = 0;
        old = new;
        new = (GC_word *)(old[1]);
    }
    GC_gcollect();
    GC_noop(x);
}

int fail_count = 0;

#ifndef __STDC__
/*ARGSUSED*/
void fail_proc1(x)
GC_PTR x;
{
    fail_count++;
}

#else

/*ARGSUSED*/
void fail_proc1(GC_PTR x)
{
    fail_count++;
}   

static void uniq(void *p, ...) {
  va_list a;
  void *q[100];
  int n = 0, i, j;
  q[n++] = p;
  va_start(a,p);
  for (;(q[n] = va_arg(a,void *));n++) ;
  va_end(a);
  for (i=0; i<n; i++)
    for (j=0; j<i; j++)
      if (q[i] == q[j]) {
        GC_printf0(
              "Apparently failed to mark form some function arguments.\n"
              "Perhaps GC_push_regs was configured incorrectly?\n"
        );
	FAIL;
      }
}

#endif /* __STDC__ */

#ifdef THREADS
#   define TEST_FAIL_COUNT(n) 1
#else 
#   define TEST_FAIL_COUNT(n) (fail_count >= (n))
#endif

void run_one_test()
{
    char *x;
#   ifdef LINT
    	char *y = 0;
#   else
    	char *y = (char *)(size_t)fail_proc1;
#   endif
    DCL_LOCK_STATE;
    
#   ifdef FIND_LEAK
	(void)GC_printf0(
		"This test program is not designed for leak detection mode\n");
	(void)GC_printf0("Expect lots of problems.\n");
#   endif
    GC_FREE(0);
#   ifndef DBG_HDRS_ALL
      collectable_count += 3;
      if (GC_size(GC_malloc(7)) != 8 &&
	  GC_size(GC_malloc(7)) != MIN_WORDS * sizeof(GC_word)
	|| GC_size(GC_malloc(15)) != 16) {
	    (void)GC_printf0("GC_size produced unexpected results\n");
	    FAIL;
      }
      collectable_count += 1;
      if (GC_size(GC_malloc(0)) != MIN_WORDS * sizeof(GC_word)) {
    	(void)GC_printf1("GC_malloc(0) failed: GC_size returns %ld\n",
			 GC_size(GC_malloc(0)));
	    FAIL;
      }
      collectable_count += 1;
      if (GC_size(GC_malloc_uncollectable(0)) != MIN_WORDS * sizeof(GC_word)) {
    	(void)GC_printf0("GC_malloc_uncollectable(0) failed\n");
	    FAIL;
      }
      GC_is_valid_displacement_print_proc = fail_proc1;
      GC_is_visible_print_proc = fail_proc1;
      collectable_count += 1;
      x = GC_malloc(16);
      if (GC_base(x + 13) != x) {
    	(void)GC_printf0("GC_base(heap ptr) produced incorrect result\n");
	FAIL;
      }
#     ifndef PCR
        if (GC_base(y) != 0) {
    	  (void)GC_printf0("GC_base(fn_ptr) produced incorrect result\n");
	  FAIL;
        }
#     endif
      if (GC_same_obj(x+5, x) != x + 5) {
    	(void)GC_printf0("GC_same_obj produced incorrect result\n");
	FAIL;
      }
      if (GC_is_visible(y) != y || GC_is_visible(x) != x) {
    	(void)GC_printf0("GC_is_visible produced incorrect result\n");
	FAIL;
      }
      if (!TEST_FAIL_COUNT(1)) {
#	if!(defined(RS6000) || defined(POWERPC) || defined(IA64)) || defined(M68K)
	  /* ON RS6000s function pointers point to a descriptor in the	*/
	  /* data segment, so there should have been no failures.	*/
	  /* The same applies to IA64.  Something similar seems to	*/
	  /* be going on with NetBSD/M68K.				*/
    	  (void)GC_printf0("GC_is_visible produced wrong failure indication\n");
    	  FAIL;
#	endif
      }
      if (GC_is_valid_displacement(y) != y
        || GC_is_valid_displacement(x) != x
        || GC_is_valid_displacement(x + 3) != x + 3) {
    	(void)GC_printf0(
    		"GC_is_valid_displacement produced incorrect result\n");
	FAIL;
      }
#     if defined(__STDC__) && !defined(MSWIN32) && !defined(MSWINCE)
        /* Harder to test under Windows without a gc.h declaration.  */
        {
	  size_t i;
	  extern void *GC_memalign();

	  GC_malloc(17);
	  for (i = sizeof(GC_word); i < 512; i *= 2) {
	    GC_word result = (GC_word) GC_memalign(i, 17);
	    if (result % i != 0 || result == 0 || *(int *)result != 0) FAIL;
	  } 
	}
#     endif
#     ifndef ALL_INTERIOR_POINTERS
#      if defined(RS6000) || defined(POWERPC)
        if (!TEST_FAIL_COUNT(1)) {
#      else
        if (GC_all_interior_pointers && !TEST_FAIL_COUNT(1)
	    || !GC_all_interior_pointers && !TEST_FAIL_COUNT(2)) {
#      endif
    	  (void)GC_printf0("GC_is_valid_displacement produced wrong failure indication\n");
    	  FAIL;
        }
#     endif
#   endif /* DBG_HDRS_ALL */
    /* Test floating point alignment */
   collectable_count += 2;
	*(double *)GC_MALLOC(sizeof(double)) = 1.0;
	*(double *)GC_MALLOC(sizeof(double)) = 1.0;
#   ifdef GC_GCJ_SUPPORT
      GC_REGISTER_DISPLACEMENT(sizeof(struct fake_vtable *));
      GC_init_gcj_malloc(0, (void *)fake_gcj_mark_proc);
#   endif
    /* Make sure that fn arguments are visible to the collector.	*/
#   ifdef __STDC__
      uniq(
        GC_malloc(12), GC_malloc(12), GC_malloc(12),
        (GC_gcollect(),GC_malloc(12)),
        GC_malloc(12), GC_malloc(12), GC_malloc(12),
	(GC_gcollect(),GC_malloc(12)),
        GC_malloc(12), GC_malloc(12), GC_malloc(12),
	(GC_gcollect(),GC_malloc(12)),
        GC_malloc(12), GC_malloc(12), GC_malloc(12),
	(GC_gcollect(),GC_malloc(12)),
        GC_malloc(12), GC_malloc(12), GC_malloc(12),
	(GC_gcollect(),GC_malloc(12)),
        (void *)0);
#   endif
    /* Repeated list reversal test. */
	reverse_test();
#   ifdef PRINTSTATS
	GC_printf0("-------------Finished reverse_test\n");
#   endif
#   ifndef DBG_HDRS_ALL
      typed_test();
#     ifdef PRINTSTATS
	GC_printf0("-------------Finished typed_test\n");
#     endif
#   endif /* DBG_HDRS_ALL */
    tree_test();
    LOCK();
    n_tests++;
    UNLOCK();
#   if defined(THREADS) && defined(HANDLE_FORK)
      if (fork() == 0) {
	GC_gcollect();
	tiny_reverse_test(0);
	GC_gcollect();
	GC_printf0("Finished a child process\n");
	exit(0);
      }
#   endif
    /* GC_printf1("Finished %x\n", pthread_self()); */
}

void check_heap_stats()
{
    unsigned long max_heap_sz;
    register int i;
    int still_live;
    int late_finalize_count = 0;
    
#   ifdef VERY_SMALL_CONFIG
    /* these are something of a guess */
    if (sizeof(char *) > 4) {
        max_heap_sz = 4500000;
    } else {
    	max_heap_sz = 2800000;
    }
#   else
    if (sizeof(char *) > 4) {
        max_heap_sz = 19000000;
    } else {
    	max_heap_sz = 11000000;
    }
#   endif
#   ifndef ALIGN_DOUBLE
        /* We end up needing more small object pages. */
        max_heap_sz += 2000000;
#   endif
#   ifdef GC_DEBUG
	max_heap_sz *= 2;
#       ifdef SAVE_CALL_CHAIN
	    max_heap_sz *= 3;
#           ifdef SAVE_CALL_COUNT
		max_heap_sz += max_heap_sz * SAVE_CALL_COUNT/4;
#	    endif
#       endif
#   endif
    /* Garbage collect repeatedly so that all inaccessible objects	*/
    /* can be finalized.						*/
      while (GC_collect_a_little()) { }
      for (i = 0; i < 16; i++) {
        GC_gcollect();
        late_finalize_count += GC_invoke_finalizers();
      }
    (void)GC_printf1("Completed %lu tests\n", (unsigned long)n_tests);
    (void)GC_printf1("Allocated %lu collectable objects\n", (unsigned long)collectable_count);
    (void)GC_printf1("Allocated %lu uncollectable objects\n", (unsigned long)uncollectable_count);
    (void)GC_printf1("Allocated %lu atomic objects\n", (unsigned long)atomic_count);
    (void)GC_printf1("Allocated %lu stubborn objects\n", (unsigned long)stubborn_count);
    (void)GC_printf2("Finalized %lu/%lu objects - ",
    		     (unsigned long)finalized_count,
    		     (unsigned long)finalizable_count);
#   ifdef FINALIZE_ON_DEMAND
	if (finalized_count != late_finalize_count) {
            (void)GC_printf0("Demand finalization error\n");
	    FAIL;
	}
#   endif
    if (finalized_count > finalizable_count
        || finalized_count < finalizable_count/2) {
        (void)GC_printf0("finalization is probably broken\n");
        FAIL;
    } else {
        (void)GC_printf0("finalization is probably ok\n");
    }
    still_live = 0;
    for (i = 0; i < MAX_FINALIZED; i++) {
    	if (live_indicators[i] != 0) {
    	    still_live++;
    	}
    }
    i = finalizable_count - finalized_count - still_live;
    if (0 != i) {
        (void)GC_printf2
            ("%lu disappearing links remain and %ld more objects were not finalized\n",
             (unsigned long) still_live, (long)i);
        if (i > 10) {
	    GC_printf0("\tVery suspicious!\n");
	} else {
	    GC_printf0("\tSlightly suspicious, but probably OK.\n");
	}
    }
    (void)GC_printf1("Total number of bytes allocated is %lu\n",
    		(unsigned long)
    	           WORDS_TO_BYTES(GC_words_allocd + GC_words_allocd_before_gc));
    (void)GC_printf1("Final heap size is %lu bytes\n",
    		     (unsigned long)GC_get_heap_size());
    if (WORDS_TO_BYTES(GC_words_allocd + GC_words_allocd_before_gc)
#   ifdef VERY_SMALL_CONFIG
        < 2700000*n_tests) {
#   else
        < 33500000*n_tests) {
#   endif
        (void)GC_printf0("Incorrect execution - missed some allocations\n");
        FAIL;
    }
    if (GC_get_heap_size() > max_heap_sz*n_tests) {
        (void)GC_printf0("Unexpected heap growth - collector may be broken\n");
        FAIL;
    }
    (void)GC_printf0("Collector appears to work\n");
}

#if defined(MACOS)
void SetMinimumStack(long minSize)
{
	long newApplLimit;

	if (minSize > LMGetDefltStack())
	{
		newApplLimit = (long) GetApplLimit()
				- (minSize - LMGetDefltStack());
		SetApplLimit((Ptr) newApplLimit);
		MaxApplZone();
	}
}

#define cMinStackSpace (512L * 1024L)

#endif

#ifdef __STDC__
    void warn_proc(char *msg, GC_word p)
#else
    void warn_proc(msg, p)
    char *msg;
    GC_word p;
#endif
{
    GC_printf1(msg, (unsigned long)p);
    /*FAIL;*/
}


#if !defined(PCR) && !defined(GC_SOLARIS_THREADS) \
    && !defined(GC_WIN32_THREADS) && !defined(GC_PTHREADS) \
    || defined(LINT)
#if defined(MSWIN32) && !defined(__MINGW32__)
  int APIENTRY WinMain(HINSTANCE instance, HINSTANCE prev, LPTSTR cmd, int n)
#else
  int main()
#endif
{
#   if defined(DJGPP)
	int dummy;
#   endif
    n_tests = 0;
    
#   if defined(DJGPP)
	/* No good way to determine stack base from library; do it */
	/* manually on this platform.				   */
    	GC_stackbottom = (GC_PTR)(&dummy);
#   endif
#   if defined(MACOS)
	/* Make sure we have lots and lots of stack space. 	*/
	SetMinimumStack(cMinStackSpace);
	/* Cheat and let stdio initialize toolbox for us.	*/
	printf("Testing GC Macintosh port.\n");
#   endif
    GC_INIT();	/* Only needed on a few platforms.	*/
    (void) GC_set_warn_proc(warn_proc);
#   if (defined(MPROTECT_VDB) || defined(PROC_VDB)) \
          && !defined(MAKE_BACK_GRAPH)
      GC_enable_incremental();
      (void) GC_printf0("Switched to incremental mode\n");
#     if defined(MPROTECT_VDB)
	(void)GC_printf0("Emulating dirty bits with mprotect/signals\n");
#     else
#       ifdef PROC_VDB
	(void)GC_printf0("Reading dirty bits from /proc\n");
#       else
    (void)GC_printf0("Using DEFAULT_VDB dirty bit implementation\n");
#       endif
#      endif
#   endif
    run_one_test();
    check_heap_stats();
#   ifndef MSWINCE
    (void)fflush(stdout);
#   endif
#   ifdef LINT
	/* Entry points we should be testing, but aren't.		   */
	/* Some can be tested by defining GC_DEBUG at the top of this file */
	/* This is a bit SunOS4 specific.				   */			
	GC_noop(GC_expand_hp, GC_add_roots, GC_clear_roots,
	        GC_register_disappearing_link,
	        GC_register_finalizer_ignore_self,
		GC_debug_register_displacement,
	        GC_print_obj, GC_debug_change_stubborn,
	        GC_debug_end_stubborn_change, GC_debug_malloc_uncollectable,
	        GC_debug_free, GC_debug_realloc, GC_generic_malloc_words_small,
	        GC_init, GC_make_closure, GC_debug_invoke_finalizer,
	        GC_page_was_ever_dirty, GC_is_fresh,
		GC_malloc_ignore_off_page, GC_malloc_atomic_ignore_off_page,
		GC_set_max_heap_size, GC_get_bytes_since_gc,
		GC_get_total_bytes, GC_pre_incr, GC_post_incr);
#   endif
#   ifdef MSWIN32
      GC_win32_free_heap();
#   endif
    return(0);
}
# endif

#if defined(GC_WIN32_THREADS) && !defined(CYGWIN32)

DWORD __stdcall thr_run_one_test(void *arg)
{
  run_one_test();
  return 0;
}

#ifdef MSWINCE
HANDLE win_created_h;
HWND win_handle;

LRESULT CALLBACK window_proc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  LRESULT ret = 0;
  switch (uMsg) {
    case WM_HIBERNATE:
      GC_printf0("Received WM_HIBERNATE, calling GC_gcollect\n");
      GC_gcollect();
      break;
    case WM_CLOSE:
      GC_printf0("Received WM_CLOSE, closing window\n");
      DestroyWindow(hwnd);
      break;
    case WM_DESTROY:
      PostQuitMessage(0);
      break;
    default:
      ret = DefWindowProc(hwnd, uMsg, wParam, lParam);
      break;
  }
  return ret;
}

DWORD __stdcall thr_window(void *arg)
{
  WNDCLASS win_class = {
    CS_NOCLOSE,
    window_proc,
    0,
    0,
    GetModuleHandle(NULL),
    NULL,
    NULL,
    (HBRUSH)(COLOR_APPWORKSPACE+1),
    NULL,
    L"GCtestWindow"
  };
  MSG msg;

  if (!RegisterClass(&win_class))
    FAIL;

  win_handle = CreateWindowEx(
    0,
    L"GCtestWindow",
    L"GCtest",
    0,
    CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
    NULL,
    NULL,
    GetModuleHandle(NULL),
    NULL);

  if (win_handle == NULL)
    FAIL;

  SetEvent(win_created_h);

  ShowWindow(win_handle, SW_SHOW);
  UpdateWindow(win_handle);

  while (GetMessage(&msg, NULL, 0, 0)) {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }

  return 0;
}
#endif

#define NTEST 2

# ifdef MSWINCE
int APIENTRY GC_WinMain(HINSTANCE instance, HINSTANCE prev, LPWSTR cmd, int n)
#   else
int APIENTRY WinMain(HINSTANCE instance, HINSTANCE prev, LPSTR cmd, int n)
# endif
{
# if NTEST > 0
   HANDLE h[NTEST];
   int i;
# endif
# ifdef MSWINCE
    HANDLE win_thr_h;
# endif
  DWORD thread_id;
# if 0
    GC_enable_incremental();
# endif
  GC_init();
  InitializeCriticalSection(&incr_cs);
  (void) GC_set_warn_proc(warn_proc);
# ifdef MSWINCE
    win_created_h = CreateEvent(NULL, FALSE, FALSE, NULL);
    if (win_created_h == (HANDLE)NULL) {
      (void)GC_printf1("Event creation failed %lu\n", (unsigned long)GetLastError());
      FAIL;
    }
    win_thr_h = GC_CreateThread(NULL, 0, thr_window, 0, 0, &thread_id);
    if (win_thr_h == (HANDLE)NULL) {
      (void)GC_printf1("Thread creation failed %lu\n", (unsigned long)GetLastError());
      FAIL;
    }
    if (WaitForSingleObject(win_created_h, INFINITE) != WAIT_OBJECT_0)
      FAIL;
    CloseHandle(win_created_h);
# endif
# if NTEST > 0
   for (i = 0; i < NTEST; i++) {
    h[i] = GC_CreateThread(NULL, 0, thr_run_one_test, 0, 0, &thread_id);
    if (h[i] == (HANDLE)NULL) {
      (void)GC_printf1("Thread creation failed %lu\n", (unsigned long)GetLastError());
      FAIL;
    }
   }
# endif /* NTEST > 0 */
  run_one_test();
# if NTEST > 0
   for (i = 0; i < NTEST; i++) {
    if (WaitForSingleObject(h[i], INFINITE) != WAIT_OBJECT_0) {
      (void)GC_printf1("Thread wait failed %lu\n", (unsigned long)GetLastError());
      FAIL;
    }
   }
# endif /* NTEST > 0 */
# ifdef MSWINCE
    PostMessage(win_handle, WM_CLOSE, 0, 0);
    if (WaitForSingleObject(win_thr_h, INFINITE) != WAIT_OBJECT_0)
      FAIL;
# endif
  check_heap_stats();
  return(0);
}

#endif /* GC_WIN32_THREADS */


#ifdef PCR
test()
{
    PCR_Th_T * th1;
    PCR_Th_T * th2;
    int code;

    n_tests = 0;
    /* GC_enable_incremental(); */
    (void) GC_set_warn_proc(warn_proc);
    th1 = PCR_Th_Fork(run_one_test, 0);
    th2 = PCR_Th_Fork(run_one_test, 0);
    run_one_test();
    if (PCR_Th_T_Join(th1, &code, NIL, PCR_allSigsBlocked, PCR_waitForever)
        != PCR_ERes_okay || code != 0) {
        (void)GC_printf0("Thread 1 failed\n");
    }
    if (PCR_Th_T_Join(th2, &code, NIL, PCR_allSigsBlocked, PCR_waitForever)
        != PCR_ERes_okay || code != 0) {
        (void)GC_printf0("Thread 2 failed\n");
    }
    check_heap_stats();
    return(0);
}
#endif

#if defined(GC_SOLARIS_THREADS) || defined(GC_PTHREADS)
void * thr_run_one_test(void * arg)
{
    run_one_test();
    return(0);
}

#ifdef GC_DEBUG
#  define GC_free GC_debug_free
#endif

#if defined(GC_SOLARIS_THREADS) && !defined(GC_SOLARIS_PTHREADS)
main()
{
    thread_t th1;
    thread_t th2;
    int code;

    n_tests = 0;
    GC_INIT();	/* Only needed if gc is dynamic library.	*/
#   ifndef MAKE_BACK_GRAPH
      GC_enable_incremental();
#   endif
    (void) GC_set_warn_proc(warn_proc);
    if (thr_keycreate(&fl_key, GC_free) != 0) {
        (void)GC_printf1("Key creation failed %lu\n", (unsigned long)code);
    	FAIL;
    }
    if ((code = thr_create(0, 1024*1024, thr_run_one_test, 0, 0, &th1)) != 0) {
    	(void)GC_printf1("Thread 1 creation failed %lu\n", (unsigned long)code);
    	FAIL;
    }
    if ((code = thr_create(0, 1024*1024, thr_run_one_test, 0, THR_NEW_LWP, &th2)) != 0) {
    	(void)GC_printf1("Thread 2 creation failed %lu\n", (unsigned long)code);
    	FAIL;
    }
    run_one_test();
    if ((code = thr_join(th1, 0, 0)) != 0) {
        (void)GC_printf1("Thread 1 failed %lu\n", (unsigned long)code);
        FAIL;
    }
    if (thr_join(th2, 0, 0) != 0) {
        (void)GC_printf1("Thread 2 failed %lu\n", (unsigned long)code);
        FAIL;
    }
    check_heap_stats();
    (void)fflush(stdout);
    return(0);
}
#else /* pthreads */

#ifndef GC_PTHREADS
  --> bad news
#endif

main()
{
    pthread_t th1;
    pthread_t th2;
    pthread_attr_t attr;
    int code;

#   ifdef GC_IRIX_THREADS
	/* Force a larger stack to be preallocated      */
	/* Since the initial cant always grow later.	*/
	*((volatile char *)&code - 1024*1024) = 0;      /* Require 1 Mb */
#   endif /* GC_IRIX_THREADS */
#   if defined(GC_HPUX_THREADS)
	/* Default stack size is too small, especially with the 64 bit ABI */
	/* Increase it.							   */
	if (pthread_default_stacksize_np(1024*1024, 0) != 0) {
          (void)GC_printf0("pthread_default_stacksize_np failed.\n");
	}
#   endif	/* GC_HPUX_THREADS */
    GC_INIT();

    pthread_attr_init(&attr);
#   if defined(GC_IRIX_THREADS) || defined(GC_FREEBSD_THREADS) \
    	|| defined(GC_DARWIN_THREADS) || defined(GC_AIX_THREADS)
    	pthread_attr_setstacksize(&attr, 1000000);
#   endif
    n_tests = 0;
#   if (defined(MPROTECT_VDB)) \
            && !defined(PARALLEL_MARK) &&!defined(REDIRECT_MALLOC) \
            && !defined(MAKE_BACK_GRAPH)
    	GC_enable_incremental();
        (void) GC_printf0("Switched to incremental mode\n");
#     if defined(MPROTECT_VDB)
        (void)GC_printf0("Emulating dirty bits with mprotect/signals\n");
#     else
#       ifdef PROC_VDB
            (void)GC_printf0("Reading dirty bits from /proc\n");
#       else
            (void)GC_printf0("Using DEFAULT_VDB dirty bit implementation\n");
#       endif
#     endif
#   endif
    (void) GC_set_warn_proc(warn_proc);
    if ((code = pthread_key_create(&fl_key, 0)) != 0) {
        (void)GC_printf1("Key creation failed %lu\n", (unsigned long)code);
    	FAIL;
    }
    if ((code = pthread_create(&th1, &attr, thr_run_one_test, 0)) != 0) {
    	(void)GC_printf1("Thread 1 creation failed %lu\n", (unsigned long)code);
    	FAIL;
    }
    if ((code = pthread_create(&th2, &attr, thr_run_one_test, 0)) != 0) {
    	(void)GC_printf1("Thread 2 creation failed %lu\n", (unsigned long)code);
    	FAIL;
    }
    run_one_test();
    if ((code = pthread_join(th1, 0)) != 0) {
        (void)GC_printf1("Thread 1 failed %lu\n", (unsigned long)code);
        FAIL;
    }
    if (pthread_join(th2, 0) != 0) {
        (void)GC_printf1("Thread 2 failed %lu\n", (unsigned long)code);
        FAIL;
    }
    check_heap_stats();
    (void)fflush(stdout);
    pthread_attr_destroy(&attr);
    GC_printf1("Completed %d collections\n", GC_gc_no);
    return(0);
}
#endif /* GC_PTHREADS */
#endif /* GC_SOLARIS_THREADS || GC_PTHREADS */
