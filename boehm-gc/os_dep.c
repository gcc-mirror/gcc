/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1995 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1999 by Hewlett-Packard Company.  All rights reserved.
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

# include "private/gc_priv.h"

# if defined(LINUX) && !defined(POWERPC)
#   include <linux/version.h>
#   if (LINUX_VERSION_CODE <= 0x10400)
      /* Ugly hack to get struct sigcontext_struct definition.  Required      */
      /* for some early 1.3.X releases.  Will hopefully go away soon. */
      /* in some later Linux releases, asm/sigcontext.h may have to   */
      /* be included instead.                                         */
#     define __KERNEL__
#     include <asm/signal.h>
#     undef __KERNEL__
#   else
      /* Kernels prior to 2.1.1 defined struct sigcontext_struct instead of */
      /* struct sigcontext.  libc6 (glibc2) uses "struct sigcontext" in     */
      /* prototypes, so we have to include the top-level sigcontext.h to    */
      /* make sure the former gets defined to be the latter if appropriate. */
#     include <features.h>
#     if 2 <= __GLIBC__
#       if 2 == __GLIBC__ && 0 == __GLIBC_MINOR__
	  /* glibc 2.1 no longer has sigcontext.h.  But signal.h	*/
	  /* has the right declaration for glibc 2.1.			*/
#         include <sigcontext.h>
#       endif /* 0 == __GLIBC_MINOR__ */
#     else /* not 2 <= __GLIBC__ */
        /* libc5 doesn't have <sigcontext.h>: go directly with the kernel   */
        /* one.  Check LINUX_VERSION_CODE to see which we should reference. */
#       include <asm/sigcontext.h>
#     endif /* 2 <= __GLIBC__ */
#   endif
# endif
# if !defined(OS2) && !defined(PCR) && !defined(AMIGA) && !defined(MACOS) \
    && !defined(MSWINCE)
#   include <sys/types.h>
#   if !defined(MSWIN32) && !defined(SUNOS4)
#   	include <unistd.h>
#   endif
# endif

# include <stdio.h>
# if defined(MSWINCE)
#   define SIGSEGV 0 /* value is irrelevant */
# else
#   include <signal.h>
# endif

/* Blatantly OS dependent routines, except for those that are related 	*/
/* to dynamic loading.							*/

# if defined(HEURISTIC2) || defined(SEARCH_FOR_DATA_START)
#   define NEED_FIND_LIMIT
# endif

# if !defined(STACKBOTTOM) && defined(HEURISTIC2)
#   define NEED_FIND_LIMIT
# endif

# if (defined(SUNOS4) && defined(DYNAMIC_LOADING)) && !defined(PCR)
#   define NEED_FIND_LIMIT
# endif

# if (defined(SVR4) || defined(AUX) || defined(DGUX) \
      || (defined(LINUX) && defined(SPARC))) && !defined(PCR)
#   define NEED_FIND_LIMIT
# endif

#ifdef NEED_FIND_LIMIT
#   include <setjmp.h>
#endif

#if defined(FREEBSD) && defined(I386)
#  include <machine/trap.h>
#endif

#ifdef AMIGA
# define GC_AMIGA_DEF
# include "AmigaOS.c"
# undef GC_AMIGA_DEF
#endif

#if defined(MSWIN32) || defined(MSWINCE)
# define WIN32_LEAN_AND_MEAN
# define NOSERVICE
# include <windows.h>
#endif

#ifdef MACOS
# include <Processes.h>
#endif

#ifdef IRIX5
# include <sys/uio.h>
# include <malloc.h>   /* for locking */
#endif
#ifdef USE_MMAP
# include <sys/types.h>
# include <sys/mman.h>
# include <sys/stat.h>
#endif

#ifdef UNIX_LIKE
# include <fcntl.h>
#endif

#if defined(SUNOS5SIGS) || defined (HURD) || defined(LINUX)
# ifdef SUNOS5SIGS
#  include <sys/siginfo.h>
# endif
# undef setjmp
# undef longjmp
# define setjmp(env) sigsetjmp(env, 1)
# define longjmp(env, val) siglongjmp(env, val)
# define jmp_buf sigjmp_buf
#endif

#ifdef DJGPP
  /* Apparently necessary for djgpp 2.01.  May cause problems with	*/
  /* other versions.							*/
  typedef long unsigned int caddr_t;
#endif

#ifdef PCR
# include "il/PCR_IL.h"
# include "th/PCR_ThCtl.h"
# include "mm/PCR_MM.h"
#endif

#if !defined(NO_EXECUTE_PERMISSION)
# define OPT_PROT_EXEC PROT_EXEC
#else
# define OPT_PROT_EXEC 0
#endif

#if defined(SEARCH_FOR_DATA_START)
  /* The I386 case can be handled without a search.  The Alpha case	*/
  /* used to be handled differently as well, but the rules changed	*/
  /* for recent Linux versions.  This seems to be the easiest way to	*/
  /* cover all versions.						*/

# ifdef LINUX
#   pragma weak __data_start
    extern int __data_start[];
#   pragma weak data_start
    extern int data_start[];
# endif /* LINUX */
  extern int _end[];

  ptr_t GC_data_start;

  void GC_init_linux_data_start()
  {
    extern ptr_t GC_find_limit();

#   ifdef LINUX
      /* Try the easy approaches first:	*/
      if (__data_start != 0) {
	  GC_data_start = (ptr_t)__data_start;
	  return;
      }
      if (data_start != 0) {
	  GC_data_start = (ptr_t)data_start;
	  return;
      }
#   endif /* LINUX */
    GC_data_start = GC_find_limit((ptr_t)_end, FALSE);
  }
#endif

# ifdef ECOS

# ifndef ECOS_GC_MEMORY_SIZE
# define ECOS_GC_MEMORY_SIZE (448 * 1024)
# endif /* ECOS_GC_MEMORY_SIZE */

// setjmp() function, as described in ANSI para 7.6.1.1
#define setjmp( __env__ )  hal_setjmp( __env__ )

// FIXME: This is a simple way of allocating memory which is
// compatible with ECOS early releases.  Later releases use a more
// sophisticated means of allocating memory than this simple static
// allocator, but this method is at least bound to work.
static char memory[ECOS_GC_MEMORY_SIZE];
static char *brk = memory;

static void *tiny_sbrk(ptrdiff_t increment)
{
  void *p = brk;

  brk += increment;

  if (brk >  memory + sizeof memory)
    {
      brk -= increment;
      return NULL;
    }

  return p;
}
#define sbrk tiny_sbrk
# endif /* ECOS */

#if (defined(NETBSD) || defined(OPENBSD)) && defined(__ELF__)
  ptr_t GC_data_start;

  void GC_init_netbsd_elf()
  {
    extern ptr_t GC_find_limit();
    extern char **environ;
	/* This may need to be environ, without the underscore, for	*/
	/* some versions.						*/
    GC_data_start = GC_find_limit((ptr_t)&environ, FALSE);
  }
#endif

# ifdef OS2

# include <stddef.h>

# if !defined(__IBMC__) && !defined(__WATCOMC__) /* e.g. EMX */

struct exe_hdr {
    unsigned short      magic_number;
    unsigned short      padding[29];
    long                new_exe_offset;
};

#define E_MAGIC(x)      (x).magic_number
#define EMAGIC          0x5A4D  
#define E_LFANEW(x)     (x).new_exe_offset

struct e32_exe {
    unsigned char       magic_number[2]; 
    unsigned char       byte_order; 
    unsigned char       word_order; 
    unsigned long       exe_format_level;
    unsigned short      cpu;       
    unsigned short      os;
    unsigned long       padding1[13];
    unsigned long       object_table_offset;
    unsigned long       object_count;    
    unsigned long       padding2[31];
};

#define E32_MAGIC1(x)   (x).magic_number[0]
#define E32MAGIC1       'L'
#define E32_MAGIC2(x)   (x).magic_number[1]
#define E32MAGIC2       'X'
#define E32_BORDER(x)   (x).byte_order
#define E32LEBO         0
#define E32_WORDER(x)   (x).word_order
#define E32LEWO         0
#define E32_CPU(x)      (x).cpu
#define E32CPU286       1
#define E32_OBJTAB(x)   (x).object_table_offset
#define E32_OBJCNT(x)   (x).object_count

struct o32_obj {
    unsigned long       size;  
    unsigned long       base;
    unsigned long       flags;  
    unsigned long       pagemap;
    unsigned long       mapsize; 
    unsigned long       reserved;
};

#define O32_FLAGS(x)    (x).flags
#define OBJREAD         0x0001L
#define OBJWRITE        0x0002L
#define OBJINVALID      0x0080L
#define O32_SIZE(x)     (x).size
#define O32_BASE(x)     (x).base

# else  /* IBM's compiler */

/* A kludge to get around what appears to be a header file bug */
# ifndef WORD
#   define WORD unsigned short
# endif
# ifndef DWORD
#   define DWORD unsigned long
# endif

# define EXE386 1
# include <newexe.h>
# include <exe386.h>

# endif  /* __IBMC__ */

# define INCL_DOSEXCEPTIONS
# define INCL_DOSPROCESS
# define INCL_DOSERRORS
# define INCL_DOSMODULEMGR
# define INCL_DOSMEMMGR
# include <os2.h>


/* Disable and enable signals during nontrivial allocations	*/

void GC_disable_signals(void)
{
    ULONG nest;
    
    DosEnterMustComplete(&nest);
    if (nest != 1) ABORT("nested GC_disable_signals");
}

void GC_enable_signals(void)
{
    ULONG nest;
    
    DosExitMustComplete(&nest);
    if (nest != 0) ABORT("GC_enable_signals");
}


# else

#  if !defined(PCR) && !defined(AMIGA) && !defined(MSWIN32) \
      && !defined(MSWINCE) \
      && !defined(MACOS) && !defined(DJGPP) && !defined(DOS4GW) \
      && !defined(NOSYS) && !defined(ECOS)

#   if defined(sigmask) && !defined(UTS4) && !defined(HURD)
	/* Use the traditional BSD interface */
#	define SIGSET_T int
#	define SIG_DEL(set, signal) (set) &= ~(sigmask(signal))
#	define SIG_FILL(set)  (set) = 0x7fffffff
    	  /* Setting the leading bit appears to provoke a bug in some	*/
    	  /* longjmp implementations.  Most systems appear not to have	*/
    	  /* a signal 32.						*/
#	define SIGSETMASK(old, new) (old) = sigsetmask(new)
#   else
	/* Use POSIX/SYSV interface	*/
#	define SIGSET_T sigset_t
#	define SIG_DEL(set, signal) sigdelset(&(set), (signal))
#	define SIG_FILL(set) sigfillset(&set)
#	define SIGSETMASK(old, new) sigprocmask(SIG_SETMASK, &(new), &(old))
#   endif

static GC_bool mask_initialized = FALSE;

static SIGSET_T new_mask;

static SIGSET_T old_mask;

static SIGSET_T dummy;

#if defined(PRINTSTATS) && !defined(THREADS)
# define CHECK_SIGNALS
  int GC_sig_disabled = 0;
#endif

void GC_disable_signals()
{
    if (!mask_initialized) {
    	SIG_FILL(new_mask);

	SIG_DEL(new_mask, SIGSEGV);
	SIG_DEL(new_mask, SIGILL);
	SIG_DEL(new_mask, SIGQUIT);
#	ifdef SIGBUS
	    SIG_DEL(new_mask, SIGBUS);
#	endif
#	ifdef SIGIOT
	    SIG_DEL(new_mask, SIGIOT);
#	endif
#	ifdef SIGEMT
	    SIG_DEL(new_mask, SIGEMT);
#	endif
#	ifdef SIGTRAP
	    SIG_DEL(new_mask, SIGTRAP);
#	endif 
	mask_initialized = TRUE;
    }
#   ifdef CHECK_SIGNALS
	if (GC_sig_disabled != 0) ABORT("Nested disables");
	GC_sig_disabled++;
#   endif
    SIGSETMASK(old_mask,new_mask);
}

void GC_enable_signals()
{
#   ifdef CHECK_SIGNALS
	if (GC_sig_disabled != 1) ABORT("Unmatched enable");
	GC_sig_disabled--;
#   endif
    SIGSETMASK(dummy,old_mask);
}

#  endif  /* !PCR */

# endif /*!OS/2 */

/* Ivan Demakov: simplest way (to me) */
#if defined (DOS4GW)
  void GC_disable_signals() { }
  void GC_enable_signals() { }
#endif

/* Find the page size */
word GC_page_size;

# if defined(MSWIN32) || defined(MSWINCE)
  void GC_setpagesize()
  {
    GetSystemInfo(&GC_sysinfo);
    GC_page_size = GC_sysinfo.dwPageSize;
  }

# else
#   if defined(MPROTECT_VDB) || defined(PROC_VDB) || defined(USE_MMAP) \
       || defined(USE_MUNMAP)
	void GC_setpagesize()
	{
	    GC_page_size = GETPAGESIZE();
	}
#   else
	/* It's acceptable to fake it. */
	void GC_setpagesize()
	{
	    GC_page_size = HBLKSIZE;
	}
#   endif
# endif

/* 
 * Find the base of the stack. 
 * Used only in single-threaded environment.
 * With threads, GC_mark_roots needs to know how to do this.
 * Called with allocator lock held.
 */
# if defined(MSWIN32) || defined(MSWINCE)
# define is_writable(prot) ((prot) == PAGE_READWRITE \
			    || (prot) == PAGE_WRITECOPY \
			    || (prot) == PAGE_EXECUTE_READWRITE \
			    || (prot) == PAGE_EXECUTE_WRITECOPY)
/* Return the number of bytes that are writable starting at p.	*/
/* The pointer p is assumed to be page aligned.			*/
/* If base is not 0, *base becomes the beginning of the 	*/
/* allocation region containing p.				*/
word GC_get_writable_length(ptr_t p, ptr_t *base)
{
    MEMORY_BASIC_INFORMATION buf;
    word result;
    word protect;
    
    result = VirtualQuery(p, &buf, sizeof(buf));
    if (result != sizeof(buf)) ABORT("Weird VirtualQuery result");
    if (base != 0) *base = (ptr_t)(buf.AllocationBase);
    protect = (buf.Protect & ~(PAGE_GUARD | PAGE_NOCACHE));
    if (!is_writable(protect)) {
        return(0);
    }
    if (buf.State != MEM_COMMIT) return(0);
    return(buf.RegionSize);
}

ptr_t GC_get_stack_base()
{
    int dummy;
    ptr_t sp = (ptr_t)(&dummy);
    ptr_t trunc_sp = (ptr_t)((word)sp & ~(GC_page_size - 1));
    word size = GC_get_writable_length(trunc_sp, 0);
   
    return(trunc_sp + size);
}


# endif /* MS Windows */

# ifdef BEOS
# include <kernel/OS.h>
ptr_t GC_get_stack_base(){
	thread_info th;
	get_thread_info(find_thread(NULL),&th);
	return th.stack_end;
}
# endif /* BEOS */


# ifdef OS2

ptr_t GC_get_stack_base()
{
    PTIB ptib;
    PPIB ppib;
    
    if (DosGetInfoBlocks(&ptib, &ppib) != NO_ERROR) {
    	GC_err_printf0("DosGetInfoBlocks failed\n");
    	ABORT("DosGetInfoBlocks failed\n");
    }
    return((ptr_t)(ptib -> tib_pstacklimit));
}

# endif /* OS2 */

# ifdef AMIGA
#   define GC_AMIGA_SB
#   include "AmigaOS.c"
#   undef GC_AMIGA_SB
# endif /* AMIGA */

# if defined(NEED_FIND_LIMIT) || defined(UNIX_LIKE)

#   ifdef __STDC__
	typedef void (*handler)(int);
#   else
	typedef void (*handler)();
#   endif

#   if defined(SUNOS5SIGS) || defined(IRIX5) || defined(OSF1) || defined(HURD)
	static struct sigaction old_segv_act;
#	if defined(_sigargs) /* !Irix6.x */ || defined(HPUX) || defined(HURD)
	    static struct sigaction old_bus_act;
#	endif
#   else
        static handler old_segv_handler, old_bus_handler;
#   endif
    
#   ifdef __STDC__
      void GC_set_and_save_fault_handler(handler h)
#   else
      void GC_set_and_save_fault_handler(h)
      handler h;
#   endif
    {
#     if defined(SUNOS5SIGS) || defined(IRIX5)  \
        || defined(OSF1) || defined(HURD)
	  struct sigaction	act;

	  act.sa_handler	= h;
#	  ifdef SUNOS5SIGS
            act.sa_flags          = SA_RESTART | SA_NODEFER;
#         else
            act.sa_flags          = SA_RESTART;
#	  endif
          /* The presence of SA_NODEFER represents yet another gross    */
          /* hack.  Under Solaris 2.3, siglongjmp doesn't appear to     */
          /* interact correctly with -lthread.  We hide the confusion   */
          /* by making sure that signal handling doesn't affect the     */
          /* signal mask.                                               */

	  (void) sigemptyset(&act.sa_mask);
#	  ifdef GC_IRIX_THREADS
		/* Older versions have a bug related to retrieving and	*/
		/* and setting a handler at the same time.		*/
	        (void) sigaction(SIGSEGV, 0, &old_segv_act);
	        (void) sigaction(SIGSEGV, &act, 0);
#	  else
	        (void) sigaction(SIGSEGV, &act, &old_segv_act);
#		if defined(IRIX5) && defined(_sigargs) /* Irix 5.x, not 6.x */ \
		   || defined(HPUX) || defined(HURD)
		    /* Under Irix 5.x or HP/UX, we may get SIGBUS.	*/
		    /* Pthreads doesn't exist under Irix 5.x, so we	*/
		    /* don't have to worry in the threads case.		*/
		    (void) sigaction(SIGBUS, &act, &old_bus_act);
#		endif
#	  endif	/* GC_IRIX_THREADS */
#	else
    	  old_segv_handler = signal(SIGSEGV, h);
#	  ifdef SIGBUS
	    old_bus_handler = signal(SIGBUS, h);
#	  endif
#	endif
    }
# endif /* NEED_FIND_LIMIT || UNIX_LIKE */

# ifdef NEED_FIND_LIMIT
  /* Some tools to implement HEURISTIC2	*/
#   define MIN_PAGE_SIZE 256	/* Smallest conceivable page size, bytes */
    /* static */ jmp_buf GC_jmp_buf;
    
    /*ARGSUSED*/
    void GC_fault_handler(sig)
    int sig;
    {
        longjmp(GC_jmp_buf, 1);
    }

    void GC_setup_temporary_fault_handler()
    {
	GC_set_and_save_fault_handler(GC_fault_handler);
    }
    
    void GC_reset_fault_handler()
    {
#     if defined(SUNOS5SIGS) || defined(IRIX5) \
	 || defined(OSF1) || defined(HURD)
	(void) sigaction(SIGSEGV, &old_segv_act, 0);
#	if defined(IRIX5) && defined(_sigargs) /* Irix 5.x, not 6.x */ \
	   || defined(HPUX) || defined(HURD)
	    (void) sigaction(SIGBUS, &old_bus_act, 0);
#	endif
#      else
	(void) signal(SIGSEGV, old_segv_handler);
#	ifdef SIGBUS
	  (void) signal(SIGBUS, old_bus_handler);
#	endif
#     endif
    }

    /* Return the first nonaddressible location > p (up) or 	*/
    /* the smallest location q s.t. [q,p] is addressible (!up).	*/
    ptr_t GC_find_limit(p, up)
    ptr_t p;
    GC_bool up;
    {
      static VOLATILE ptr_t result;
  		/* Needs to be static, since otherwise it may not be	*/
  		/* preserved across the longjmp.  Can safely be 	*/
  		/* static since it's only called once, with the		*/
  		/* allocation lock held.				*/


      GC_setup_temporary_fault_handler();
      if (setjmp(GC_jmp_buf) == 0) {
	result = (ptr_t)(((word)(p))
			 & ~(MIN_PAGE_SIZE-1));
	for (;;) {
	  if (up) {
	    result += MIN_PAGE_SIZE;
	  } else {
	    result -= MIN_PAGE_SIZE;
	  }
	  GC_noop1((word)(*result));
	}
      }
      GC_reset_fault_handler();
      if (!up) {
	result += MIN_PAGE_SIZE;
      }
      return(result);
    }
# endif

# if defined(ECOS) || defined(NOSYS)
ptr_t GC_get_stack_base()
{
  return STACKBOTTOM;
}

#else

#ifdef LINUX_STACKBOTTOM

#include <sys/types.h>
#include <sys/stat.h>

# define STAT_SKIP 27   /* Number of fields preceding startstack	*/
			/* field in /proc/self/stat			*/

# pragma weak __libc_stack_end
  extern ptr_t __libc_stack_end;

# ifdef IA64
#   pragma weak __libc_ia64_register_backing_store_base
    extern ptr_t __libc_ia64_register_backing_store_base;

    ptr_t GC_get_register_stack_base(void)
    {
      if (0 != &__libc_ia64_register_backing_store_base
	  && 0 != __libc_ia64_register_backing_store_base) {
	/* Glibc 2.2.4 has a bug such that for dynamically linked	*/
	/* executables __libc_ia64_register_backing_store_base is 	*/
	/* defined but ininitialized during constructor calls.  	*/
	/* Hence we check for both nonzero address and value.		*/
	return __libc_ia64_register_backing_store_base;
      } else {
	word result = (word)GC_stackbottom - BACKING_STORE_DISPLACEMENT;
	result += BACKING_STORE_ALIGNMENT - 1;
	result &= ~(BACKING_STORE_ALIGNMENT - 1);
	return (ptr_t)result;
      }
    }
# endif

  ptr_t GC_linux_stack_base(void)
  {
    /* We read the stack base value from /proc/self/stat.  We do this	*/
    /* using direct I/O system calls in order to avoid calling malloc   */
    /* in case REDIRECT_MALLOC is defined.				*/ 
#   define STAT_BUF_SIZE 4096
#   if defined(GC_USE_LD_WRAP)
#	define STAT_READ __real_read
#   else
#	define STAT_READ read
#   endif    
    char stat_buf[STAT_BUF_SIZE];
    int f;
    char c;
    word result = 0;
    size_t i, buf_offset = 0;

    /* First try the easy way.  This should work for glibc 2.2	*/
      if (0 != &__libc_stack_end) {
	return __libc_stack_end;
      }
    f = open("/proc/self/stat", O_RDONLY);
    if (f < 0 || STAT_READ(f, stat_buf, STAT_BUF_SIZE) < 2 * STAT_SKIP) {
	ABORT("Couldn't read /proc/self/stat");
    }
    c = stat_buf[buf_offset++];
    /* Skip the required number of fields.  This number is hopefully	*/
    /* constant across all Linux implementations.			*/
      for (i = 0; i < STAT_SKIP; ++i) {
	while (isspace(c)) c = stat_buf[buf_offset++];
	while (!isspace(c)) c = stat_buf[buf_offset++];
      }
    while (isspace(c)) c = stat_buf[buf_offset++];
    while (isdigit(c)) {
      result *= 10;
      result += c - '0';
      c = stat_buf[buf_offset++];
    }
    close(f);
    if (result < 0x10000000) ABORT("Absurd stack bottom value");
    return (ptr_t)result;
  }

#endif /* LINUX_STACKBOTTOM */

#ifdef FREEBSD_STACKBOTTOM

/* This uses an undocumented sysctl call, but at least one expert 	*/
/* believes it will stay.						*/

#include <unistd.h>
#include <sys/types.h>
#include <sys/sysctl.h>

  ptr_t GC_freebsd_stack_base(void)
  {
    int nm[2] = {CTL_KERN, KERN_USRSTACK};
    ptr_t base;
    size_t len = sizeof(ptr_t);
    int r = sysctl(nm, 2, &base, &len, NULL, 0);
    
    if (r) ABORT("Error getting stack base");

    return base;
  }

#endif /* FREEBSD_STACKBOTTOM */

#if !defined(BEOS) && !defined(AMIGA) && !defined(MSWIN32) \
    && !defined(MSWINCE) && !defined(OS2)

ptr_t GC_get_stack_base()
{
    word dummy;
    ptr_t result;

#   define STACKBOTTOM_ALIGNMENT_M1 ((word)STACK_GRAN - 1)

#   ifdef STACKBOTTOM
	return(STACKBOTTOM);
#   else
#	ifdef HEURISTIC1
#	   ifdef STACK_GROWS_DOWN
	     result = (ptr_t)((((word)(&dummy))
	     		       + STACKBOTTOM_ALIGNMENT_M1)
			      & ~STACKBOTTOM_ALIGNMENT_M1);
#	   else
	     result = (ptr_t)(((word)(&dummy))
			      & ~STACKBOTTOM_ALIGNMENT_M1);
#	   endif
#	endif /* HEURISTIC1 */
#	ifdef LINUX_STACKBOTTOM
	   result = GC_linux_stack_base();
#	endif
#	ifdef FREEBSD_STACKBOTTOM
	   result = GC_freebsd_stack_base();
#	endif
#	ifdef HEURISTIC2
#	    ifdef STACK_GROWS_DOWN
		result = GC_find_limit((ptr_t)(&dummy), TRUE);
#           	ifdef HEURISTIC2_LIMIT
		    if (result > HEURISTIC2_LIMIT
		        && (ptr_t)(&dummy) < HEURISTIC2_LIMIT) {
		            result = HEURISTIC2_LIMIT;
		    }
#	        endif
#	    else
		result = GC_find_limit((ptr_t)(&dummy), FALSE);
#           	ifdef HEURISTIC2_LIMIT
		    if (result < HEURISTIC2_LIMIT
		        && (ptr_t)(&dummy) > HEURISTIC2_LIMIT) {
		            result = HEURISTIC2_LIMIT;
		    }
#	        endif
#	    endif

#	endif /* HEURISTIC2 */
#	ifdef STACK_GROWS_DOWN
	    if (result == 0) result = (ptr_t)(signed_word)(-sizeof(ptr_t));
#	endif
    	return(result);
#   endif /* STACKBOTTOM */
}
# endif /* NOSYS ECOS */

# endif /* ! AMIGA, !OS 2, ! MS Windows, !BEOS */

/*
 * Register static data segment(s) as roots.
 * If more data segments are added later then they need to be registered
 * add that point (as we do with SunOS dynamic loading),
 * or GC_mark_roots needs to check for them (as we do with PCR).
 * Called with allocator lock held.
 */

# ifdef OS2

void GC_register_data_segments()
{
    PTIB ptib;
    PPIB ppib;
    HMODULE module_handle;
#   define PBUFSIZ 512
    UCHAR path[PBUFSIZ];
    FILE * myexefile;
    struct exe_hdr hdrdos;	/* MSDOS header.	*/
    struct e32_exe hdr386;	/* Real header for my executable */
    struct o32_obj seg;	/* Currrent segment */
    int nsegs;
    
    
    if (DosGetInfoBlocks(&ptib, &ppib) != NO_ERROR) {
    	GC_err_printf0("DosGetInfoBlocks failed\n");
    	ABORT("DosGetInfoBlocks failed\n");
    }
    module_handle = ppib -> pib_hmte;
    if (DosQueryModuleName(module_handle, PBUFSIZ, path) != NO_ERROR) {
    	GC_err_printf0("DosQueryModuleName failed\n");
    	ABORT("DosGetInfoBlocks failed\n");
    }
    myexefile = fopen(path, "rb");
    if (myexefile == 0) {
        GC_err_puts("Couldn't open executable ");
        GC_err_puts(path); GC_err_puts("\n");
        ABORT("Failed to open executable\n");
    }
    if (fread((char *)(&hdrdos), 1, sizeof hdrdos, myexefile) < sizeof hdrdos) {
        GC_err_puts("Couldn't read MSDOS header from ");
        GC_err_puts(path); GC_err_puts("\n");
        ABORT("Couldn't read MSDOS header");
    }
    if (E_MAGIC(hdrdos) != EMAGIC) {
        GC_err_puts("Executable has wrong DOS magic number: ");
        GC_err_puts(path); GC_err_puts("\n");
        ABORT("Bad DOS magic number");
    }
    if (fseek(myexefile, E_LFANEW(hdrdos), SEEK_SET) != 0) {
        GC_err_puts("Seek to new header failed in ");
        GC_err_puts(path); GC_err_puts("\n");
        ABORT("Bad DOS magic number");
    }
    if (fread((char *)(&hdr386), 1, sizeof hdr386, myexefile) < sizeof hdr386) {
        GC_err_puts("Couldn't read MSDOS header from ");
        GC_err_puts(path); GC_err_puts("\n");
        ABORT("Couldn't read OS/2 header");
    }
    if (E32_MAGIC1(hdr386) != E32MAGIC1 || E32_MAGIC2(hdr386) != E32MAGIC2) {
        GC_err_puts("Executable has wrong OS/2 magic number:");
        GC_err_puts(path); GC_err_puts("\n");
        ABORT("Bad OS/2 magic number");
    }
    if ( E32_BORDER(hdr386) != E32LEBO || E32_WORDER(hdr386) != E32LEWO) {
        GC_err_puts("Executable %s has wrong byte order: ");
        GC_err_puts(path); GC_err_puts("\n");
        ABORT("Bad byte order");
    }
    if ( E32_CPU(hdr386) == E32CPU286) {
        GC_err_puts("GC can't handle 80286 executables: ");
        GC_err_puts(path); GC_err_puts("\n");
        EXIT();
    }
    if (fseek(myexefile, E_LFANEW(hdrdos) + E32_OBJTAB(hdr386),
    	      SEEK_SET) != 0) {
        GC_err_puts("Seek to object table failed: ");
        GC_err_puts(path); GC_err_puts("\n");
        ABORT("Seek to object table failed");
    }
    for (nsegs = E32_OBJCNT(hdr386); nsegs > 0; nsegs--) {
      int flags;
      if (fread((char *)(&seg), 1, sizeof seg, myexefile) < sizeof seg) {
        GC_err_puts("Couldn't read obj table entry from ");
        GC_err_puts(path); GC_err_puts("\n");
        ABORT("Couldn't read obj table entry");
      }
      flags = O32_FLAGS(seg);
      if (!(flags & OBJWRITE)) continue;
      if (!(flags & OBJREAD)) continue;
      if (flags & OBJINVALID) {
          GC_err_printf0("Object with invalid pages?\n");
          continue;
      } 
      GC_add_roots_inner(O32_BASE(seg), O32_BASE(seg)+O32_SIZE(seg), FALSE);
    }
}

# else /* !OS2 */

# if defined(MSWIN32) || defined(MSWINCE)

# ifdef MSWIN32
  /* Unfortunately, we have to handle win32s very differently from NT, 	*/
  /* Since VirtualQuery has very different semantics.  In particular,	*/
  /* under win32s a VirtualQuery call on an unmapped page returns an	*/
  /* invalid result.  Under GC_register_data_segments is a noop and	*/
  /* all real work is done by GC_register_dynamic_libraries.  Under	*/
  /* win32s, we cannot find the data segments associated with dll's.	*/
  /* We rgister the main data segment here.				*/
#  ifdef __GCC__
  GC_bool GC_no_win32_dlls = TRUE;	 /* GCC can't do SEH, so we can't use VirtualQuery */
#  else
  GC_bool GC_no_win32_dlls = FALSE;	 
#  endif
  
  void GC_init_win32()
  {
    /* if we're running under win32s, assume that no DLLs will be loaded */
    DWORD v = GetVersion();
    GC_no_win32_dlls |= ((v & 0x80000000) && (v & 0xff) <= 3);
  }

  /* Return the smallest address a such that VirtualQuery		*/
  /* returns correct results for all addresses between a and start.	*/
  /* Assumes VirtualQuery returns correct information for start.	*/
  ptr_t GC_least_described_address(ptr_t start)
  {  
    MEMORY_BASIC_INFORMATION buf;
    DWORD result;
    LPVOID limit;
    ptr_t p;
    LPVOID q;
    
    limit = GC_sysinfo.lpMinimumApplicationAddress;
    p = (ptr_t)((word)start & ~(GC_page_size - 1));
    for (;;) {
    	q = (LPVOID)(p - GC_page_size);
    	if ((ptr_t)q > (ptr_t)p /* underflow */ || q < limit) break;
    	result = VirtualQuery(q, &buf, sizeof(buf));
    	if (result != sizeof(buf) || buf.AllocationBase == 0) break;
    	p = (ptr_t)(buf.AllocationBase);
    }
    return(p);
  }
# endif
  
  /* Is p the start of either the malloc heap, or of one of our */
  /* heap sections?						*/
  GC_bool GC_is_heap_base (ptr_t p)
  {
     
     register unsigned i;
     
#    ifndef REDIRECT_MALLOC
       static ptr_t malloc_heap_pointer = 0;
     
       if (0 == malloc_heap_pointer) {
         MEMORY_BASIC_INFORMATION buf;
         void *pTemp = malloc( 1 );
         register DWORD result = VirtualQuery(pTemp, &buf, sizeof(buf));
           
         free( pTemp );

         
         if (result != sizeof(buf)) {
             ABORT("Weird VirtualQuery result");
         }
         malloc_heap_pointer = (ptr_t)(buf.AllocationBase);
       }
       if (p == malloc_heap_pointer) return(TRUE);
#    endif
     for (i = 0; i < GC_n_heap_bases; i++) {
         if (GC_heap_bases[i] == p) return(TRUE);
     }
     return(FALSE);
  }

# ifdef MSWIN32
  void GC_register_root_section(ptr_t static_root)
  {
      MEMORY_BASIC_INFORMATION buf;
      DWORD result;
      DWORD protect;
      LPVOID p;
      char * base;
      char * limit, * new_limit;
    
      if (!GC_no_win32_dlls) return;
      p = base = limit = GC_least_described_address(static_root);
      while (p < GC_sysinfo.lpMaximumApplicationAddress) {
        result = VirtualQuery(p, &buf, sizeof(buf));
        if (result != sizeof(buf) || buf.AllocationBase == 0
            || GC_is_heap_base(buf.AllocationBase)) break;
        new_limit = (char *)p + buf.RegionSize;
        protect = buf.Protect;
        if (buf.State == MEM_COMMIT
            && is_writable(protect)) {
            if ((char *)p == limit) {
                limit = new_limit;
            } else {
                if (base != limit) GC_add_roots_inner(base, limit, FALSE);
                base = p;
                limit = new_limit;
            }
        }
        if (p > (LPVOID)new_limit /* overflow */) break;
        p = (LPVOID)new_limit;
      }
      if (base != limit) GC_add_roots_inner(base, limit, FALSE);
  }
#endif
  
  void GC_register_data_segments()
  {
#     ifdef MSWIN32
      static char dummy;
      GC_register_root_section((ptr_t)(&dummy));
#     endif
  }

# else /* !OS2 && !Windows */

# if (defined(SVR4) || defined(AUX) || defined(DGUX) \
      || (defined(LINUX) && defined(SPARC))) && !defined(PCR)
char * GC_SysVGetDataStart(max_page_size, etext_addr)
int max_page_size;
int * etext_addr;
{
    word text_end = ((word)(etext_addr) + sizeof(word) - 1)
    		    & ~(sizeof(word) - 1);
    	/* etext rounded to word boundary	*/
    word next_page = ((text_end + (word)max_page_size - 1)
    		      & ~((word)max_page_size - 1));
    word page_offset = (text_end & ((word)max_page_size - 1));
    VOLATILE char * result = (char *)(next_page + page_offset);
    /* Note that this isnt equivalent to just adding		*/
    /* max_page_size to &etext if &etext is at a page boundary	*/
    
    GC_setup_temporary_fault_handler();
    if (setjmp(GC_jmp_buf) == 0) {
    	/* Try writing to the address.	*/
    	*result = *result;
        GC_reset_fault_handler();
    } else {
        GC_reset_fault_handler();
    	/* We got here via a longjmp.  The address is not readable.	*/
    	/* This is known to happen under Solaris 2.4 + gcc, which place	*/
    	/* string constants in the text segment, but after etext.	*/
    	/* Use plan B.  Note that we now know there is a gap between	*/
    	/* text and data segments, so plan A bought us something.	*/
    	result = (char *)GC_find_limit((ptr_t)(DATAEND) - MIN_PAGE_SIZE, FALSE);
    }
    return((char *)result);
}
# endif


#ifdef AMIGA

#  define GC_AMIGA_DS
#  include "AmigaOS.c"
#  undef GC_AMIGA_DS

#else /* !OS2 && !Windows && !AMIGA */

void GC_register_data_segments()
{
#   if !defined(PCR) && !defined(SRC_M3) && !defined(NEXT) && !defined(MACOS) \
       && !defined(MACOSX)
#     if defined(REDIRECT_MALLOC) && defined(GC_SOLARIS_THREADS)
	/* As of Solaris 2.3, the Solaris threads implementation	*/
	/* allocates the data structure for the initial thread with	*/
	/* sbrk at process startup.  It needs to be scanned, so that	*/
	/* we don't lose some malloc allocated data structures		*/
	/* hanging from it.  We're on thin ice here ...			*/
        extern caddr_t sbrk();

	GC_add_roots_inner(DATASTART, (char *)sbrk(0), FALSE);
#     else
	GC_add_roots_inner(DATASTART, (char *)(DATAEND), FALSE);
#       if defined(DATASTART2)
         GC_add_roots_inner(DATASTART2, (char *)(DATAEND2), FALSE);
#       endif
#     endif
#   endif
#   if !defined(PCR) && (defined(NEXT) || defined(MACOSX))
      GC_add_roots_inner(DATASTART, (char *) get_end(), FALSE);
#   endif
#   if defined(MACOS)
    {
#   if defined(THINK_C)
	extern void* GC_MacGetDataStart(void);
	/* globals begin above stack and end at a5. */
	GC_add_roots_inner((ptr_t)GC_MacGetDataStart(),
			   (ptr_t)LMGetCurrentA5(), FALSE);
#   else
#     if defined(__MWERKS__)
#       if !__POWERPC__
	  extern void* GC_MacGetDataStart(void);
	  /* MATTHEW: Function to handle Far Globals (CW Pro 3) */
#         if __option(far_data)
	  extern void* GC_MacGetDataEnd(void);
#         endif
	  /* globals begin above stack and end at a5. */
	  GC_add_roots_inner((ptr_t)GC_MacGetDataStart(),
          		     (ptr_t)LMGetCurrentA5(), FALSE);
	  /* MATTHEW: Handle Far Globals */          		     
#         if __option(far_data)
      /* Far globals follow he QD globals: */
	  GC_add_roots_inner((ptr_t)LMGetCurrentA5(),
          		     (ptr_t)GC_MacGetDataEnd(), FALSE);
#         endif
#       else
	  extern char __data_start__[], __data_end__[];
	  GC_add_roots_inner((ptr_t)&__data_start__,
	  		     (ptr_t)&__data_end__, FALSE);
#       endif /* __POWERPC__ */
#     endif /* __MWERKS__ */
#   endif /* !THINK_C */
    }
#   endif /* MACOS */

    /* Dynamic libraries are added at every collection, since they may  */
    /* change.								*/
}

# endif  /* ! AMIGA */
# endif  /* ! MSWIN32 && ! MSWINCE*/
# endif  /* ! OS2 */

/*
 * Auxiliary routines for obtaining memory from OS.
 */

# if !defined(OS2) && !defined(PCR) && !defined(AMIGA) \
	&& !defined(MSWIN32) && !defined(MSWINCE) \
	&& !defined(MACOS) && !defined(DOS4GW)

# ifdef SUNOS4
    extern caddr_t sbrk();
# endif
# ifdef __STDC__
#   define SBRK_ARG_T ptrdiff_t
# else
#   define SBRK_ARG_T int
# endif


# ifdef RS6000
/* The compiler seems to generate speculative reads one past the end of	*/
/* an allocated object.  Hence we need to make sure that the page 	*/
/* following the last heap page is also mapped.				*/
ptr_t GC_unix_get_mem(bytes)
word bytes;
{
    caddr_t cur_brk = (caddr_t)sbrk(0);
    caddr_t result;
    SBRK_ARG_T lsbs = (word)cur_brk & (GC_page_size-1);
    static caddr_t my_brk_val = 0;
    
    if ((SBRK_ARG_T)bytes < 0) return(0); /* too big */
    if (lsbs != 0) {
        if((caddr_t)(sbrk(GC_page_size - lsbs)) == (caddr_t)(-1)) return(0);
    }
    if (cur_brk == my_brk_val) {
    	/* Use the extra block we allocated last time. */
        result = (ptr_t)sbrk((SBRK_ARG_T)bytes);
        if (result == (caddr_t)(-1)) return(0);
        result -= GC_page_size;
    } else {
        result = (ptr_t)sbrk(GC_page_size + (SBRK_ARG_T)bytes);
        if (result == (caddr_t)(-1)) return(0);
    }
    my_brk_val = result + bytes + GC_page_size;	/* Always page aligned */
    return((ptr_t)result);
}

#else  /* Not RS6000 */

#if defined(USE_MMAP)
/* Tested only under Linux, IRIX5 and Solaris 2 */

#ifdef USE_MMAP_FIXED
#   define GC_MMAP_FLAGS MAP_FIXED | MAP_PRIVATE
	/* Seems to yield better performance on Solaris 2, but can	*/
	/* be unreliable if something is already mapped at the address.	*/
#else
#   define GC_MMAP_FLAGS MAP_PRIVATE
#endif

#ifndef HEAP_START
#   define HEAP_START 0
#endif

ptr_t GC_unix_get_mem(bytes)
word bytes;
{
    static GC_bool initialized = FALSE;
    static int fd;
    void *result;
    static ptr_t last_addr = HEAP_START;

    if (!initialized) {
	fd = open("/dev/zero", O_RDONLY);
	initialized = TRUE;
    }
    if (bytes & (GC_page_size -1)) ABORT("Bad GET_MEM arg");
    result = mmap(last_addr, bytes, PROT_READ | PROT_WRITE | OPT_PROT_EXEC,
		  GC_MMAP_FLAGS, fd, 0/* offset */);
    if (result == MAP_FAILED) return(0);
    last_addr = (ptr_t)result + bytes + GC_page_size - 1;
    last_addr = (ptr_t)((word)last_addr & ~(GC_page_size - 1));
#   if !defined(LINUX)
      if (last_addr == 0) {
        /* Oops.  We got the end of the address space.  This isn't	*/
	/* usable by arbitrary C code, since one-past-end pointers	*/
	/* don't work, so we discard it and try again.			*/
	munmap(result, (size_t)(-GC_page_size) - (size_t)result);
			/* Leave last page mapped, so we can't repeat. */
	return GC_unix_get_mem(bytes);
      }
#   else
      GC_ASSERT(last_addr != 0);
#   endif
    return((ptr_t)result);
}

#else /* Not RS6000, not USE_MMAP */
ptr_t GC_unix_get_mem(bytes)
word bytes;
{
  ptr_t result;
# ifdef IRIX5
    /* Bare sbrk isn't thread safe.  Play by malloc rules.	*/
    /* The equivalent may be needed on other systems as well. 	*/
    __LOCK_MALLOC();
# endif
  {
    ptr_t cur_brk = (ptr_t)sbrk(0);
    SBRK_ARG_T lsbs = (word)cur_brk & (GC_page_size-1);
    
    if ((SBRK_ARG_T)bytes < 0) return(0); /* too big */
    if (lsbs != 0) {
        if((ptr_t)sbrk(GC_page_size - lsbs) == (ptr_t)(-1)) return(0);
    }
    result = (ptr_t)sbrk((SBRK_ARG_T)bytes);
    if (result == (ptr_t)(-1)) result = 0;
  }
# ifdef IRIX5
    __UNLOCK_MALLOC();
# endif
  return(result);
}

#endif /* Not USE_MMAP */
#endif /* Not RS6000 */

# endif /* UN*X */

# ifdef OS2

void * os2_alloc(size_t bytes)
{
    void * result;

    if (DosAllocMem(&result, bytes, PAG_EXECUTE | PAG_READ |
    				    PAG_WRITE | PAG_COMMIT)
		    != NO_ERROR) {
	return(0);
    }
    if (result == 0) return(os2_alloc(bytes));
    return(result);
}

# endif /* OS2 */


# if defined(MSWIN32) || defined(MSWINCE)
SYSTEM_INFO GC_sysinfo;
# endif

# ifdef MSWIN32

# ifdef USE_GLOBAL_ALLOC
#   define GLOBAL_ALLOC_TEST 1
# else
#   define GLOBAL_ALLOC_TEST GC_no_win32_dlls
# endif

word GC_n_heap_bases = 0;

ptr_t GC_win32_get_mem(bytes)
word bytes;
{
    ptr_t result;

    if (GLOBAL_ALLOC_TEST) {
    	/* VirtualAlloc doesn't like PAGE_EXECUTE_READWRITE.	*/
    	/* There are also unconfirmed rumors of other		*/
    	/* problems, so we dodge the issue.			*/
        result = (ptr_t) GlobalAlloc(0, bytes + HBLKSIZE);
        result = (ptr_t)(((word)result + HBLKSIZE) & ~(HBLKSIZE-1));
    } else {
        result = (ptr_t) VirtualAlloc(NULL, bytes,
    				      MEM_COMMIT | MEM_RESERVE,
    				      PAGE_EXECUTE_READWRITE);
    }
    if (HBLKDISPL(result) != 0) ABORT("Bad VirtualAlloc result");
    	/* If I read the documentation correctly, this can	*/
    	/* only happen if HBLKSIZE > 64k or not a power of 2.	*/
    if (GC_n_heap_bases >= MAX_HEAP_SECTS) ABORT("Too many heap sections");
    GC_heap_bases[GC_n_heap_bases++] = result;
    return(result);			  
}

void GC_win32_free_heap ()
{
    if (GC_no_win32_dlls) {
 	while (GC_n_heap_bases > 0) {
 	    GlobalFree (GC_heap_bases[--GC_n_heap_bases]);
 	    GC_heap_bases[GC_n_heap_bases] = 0;
 	}
    }
}
# endif

#ifdef AMIGA
# define GC_AMIGA_AM
# include "AmigaOS.c"
# undef GC_AMIGA_AM
#endif


# ifdef MSWINCE
word GC_n_heap_bases = 0;

ptr_t GC_wince_get_mem(bytes)
word bytes;
{
    ptr_t result;
    word i;

    /* Round up allocation size to multiple of page size */
    bytes = (bytes + GC_page_size-1) & ~(GC_page_size-1);

    /* Try to find reserved, uncommitted pages */
    for (i = 0; i < GC_n_heap_bases; i++) {
	if (((word)(-(signed_word)GC_heap_lengths[i])
	     & (GC_sysinfo.dwAllocationGranularity-1))
	    >= bytes) {
	    result = GC_heap_bases[i] + GC_heap_lengths[i];
	    break;
	}
    }

    if (i == GC_n_heap_bases) {
	/* Reserve more pages */
	word res_bytes = (bytes + GC_sysinfo.dwAllocationGranularity-1)
			 & ~(GC_sysinfo.dwAllocationGranularity-1);
	result = (ptr_t) VirtualAlloc(NULL, res_bytes,
    				      MEM_RESERVE | MEM_TOP_DOWN,
    				      PAGE_EXECUTE_READWRITE);
	if (HBLKDISPL(result) != 0) ABORT("Bad VirtualAlloc result");
    	    /* If I read the documentation correctly, this can	*/
    	    /* only happen if HBLKSIZE > 64k or not a power of 2.	*/
	if (GC_n_heap_bases >= MAX_HEAP_SECTS) ABORT("Too many heap sections");
	GC_heap_bases[GC_n_heap_bases] = result;
	GC_heap_lengths[GC_n_heap_bases] = 0;
	GC_n_heap_bases++;
    }

    /* Commit pages */
    result = (ptr_t) VirtualAlloc(result, bytes,
				  MEM_COMMIT,
    				  PAGE_EXECUTE_READWRITE);
    if (result != NULL) {
	if (HBLKDISPL(result) != 0) ABORT("Bad VirtualAlloc result");
	GC_heap_lengths[i] += bytes;
    }

    return(result);			  
}
# endif

#ifdef USE_MUNMAP

/* For now, this only works on Win32/WinCE and some Unix-like	*/
/* systems.  If you have something else, don't define		*/
/* USE_MUNMAP.							*/
/* We assume ANSI C to support this feature.			*/

#if !defined(MSWIN32) && !defined(MSWINCE)

#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>

#endif

/* Compute a page aligned starting address for the unmap 	*/
/* operation on a block of size bytes starting at start.	*/
/* Return 0 if the block is too small to make this feasible.	*/
ptr_t GC_unmap_start(ptr_t start, word bytes)
{
    ptr_t result = start;
    /* Round start to next page boundary.       */
        result += GC_page_size - 1;
        result = (ptr_t)((word)result & ~(GC_page_size - 1));
    if (result + GC_page_size > start + bytes) return 0;
    return result;
}

/* Compute end address for an unmap operation on the indicated	*/
/* block.							*/
ptr_t GC_unmap_end(ptr_t start, word bytes)
{
    ptr_t end_addr = start + bytes;
    end_addr = (ptr_t)((word)end_addr & ~(GC_page_size - 1));
    return end_addr;
}

/* Under Win32/WinCE we commit (map) and decommit (unmap)	*/
/* memory using	VirtualAlloc and VirtualFree.  These functions	*/
/* work on individual allocations of virtual memory, made	*/
/* previously using VirtualAlloc with the MEM_RESERVE flag.	*/
/* The ranges we need to (de)commit may span several of these	*/
/* allocations; therefore we use VirtualQuery to check		*/
/* allocation lengths, and split up the range as necessary.	*/

/* We assume that GC_remap is called on exactly the same range	*/
/* as a previous call to GC_unmap.  It is safe to consistently	*/
/* round the endpoints in both places.				*/
void GC_unmap(ptr_t start, word bytes)
{
    ptr_t start_addr = GC_unmap_start(start, bytes);
    ptr_t end_addr = GC_unmap_end(start, bytes);
    word len = end_addr - start_addr;
    if (0 == start_addr) return;
#   if defined(MSWIN32) || defined(MSWINCE)
      while (len != 0) {
          MEMORY_BASIC_INFORMATION mem_info;
	  GC_word free_len;
	  if (VirtualQuery(start_addr, &mem_info, sizeof(mem_info))
	      != sizeof(mem_info))
	      ABORT("Weird VirtualQuery result");
	  free_len = (len < mem_info.RegionSize) ? len : mem_info.RegionSize;
	  if (!VirtualFree(start_addr, free_len, MEM_DECOMMIT))
	      ABORT("VirtualFree failed");
	  GC_unmapped_bytes += free_len;
	  start_addr += free_len;
	  len -= free_len;
      }
#   else
      if (munmap(start_addr, len) != 0) ABORT("munmap failed");
      GC_unmapped_bytes += len;
#   endif
}


void GC_remap(ptr_t start, word bytes)
{
    static int zero_descr = -1;
    ptr_t start_addr = GC_unmap_start(start, bytes);
    ptr_t end_addr = GC_unmap_end(start, bytes);
    word len = end_addr - start_addr;
    ptr_t result;

#   if defined(MSWIN32) || defined(MSWINCE)
      if (0 == start_addr) return;
      while (len != 0) {
          MEMORY_BASIC_INFORMATION mem_info;
	  GC_word alloc_len;
	  if (VirtualQuery(start_addr, &mem_info, sizeof(mem_info))
	      != sizeof(mem_info))
	      ABORT("Weird VirtualQuery result");
	  alloc_len = (len < mem_info.RegionSize) ? len : mem_info.RegionSize;
	  result = VirtualAlloc(start_addr, alloc_len,
				MEM_COMMIT,
				PAGE_EXECUTE_READWRITE);
	  if (result != start_addr) {
	      ABORT("VirtualAlloc remapping failed");
	  }
	  GC_unmapped_bytes -= alloc_len;
	  start_addr += alloc_len;
	  len -= alloc_len;
      }
#   else
      if (-1 == zero_descr) zero_descr = open("/dev/zero", O_RDWR);
      if (0 == start_addr) return;
      result = mmap(start_addr, len, PROT_READ | PROT_WRITE | OPT_PROT_EXEC,
		    MAP_FIXED | MAP_PRIVATE, zero_descr, 0);
      if (result != start_addr) {
	  ABORT("mmap remapping failed");
      }
      GC_unmapped_bytes -= len;
#   endif
}

/* Two adjacent blocks have already been unmapped and are about to	*/
/* be merged.  Unmap the whole block.  This typically requires		*/
/* that we unmap a small section in the middle that was not previously	*/
/* unmapped due to alignment constraints.				*/
void GC_unmap_gap(ptr_t start1, word bytes1, ptr_t start2, word bytes2)
{
    ptr_t start1_addr = GC_unmap_start(start1, bytes1);
    ptr_t end1_addr = GC_unmap_end(start1, bytes1);
    ptr_t start2_addr = GC_unmap_start(start2, bytes2);
    ptr_t end2_addr = GC_unmap_end(start2, bytes2);
    ptr_t start_addr = end1_addr;
    ptr_t end_addr = start2_addr;
    word len;
    GC_ASSERT(start1 + bytes1 == start2);
    if (0 == start1_addr) start_addr = GC_unmap_start(start1, bytes1 + bytes2);
    if (0 == start2_addr) end_addr = GC_unmap_end(start1, bytes1 + bytes2);
    if (0 == start_addr) return;
    len = end_addr - start_addr;
#   if defined(MSWIN32) || defined(MSWINCE)
      while (len != 0) {
          MEMORY_BASIC_INFORMATION mem_info;
	  GC_word free_len;
	  if (VirtualQuery(start_addr, &mem_info, sizeof(mem_info))
	      != sizeof(mem_info))
	      ABORT("Weird VirtualQuery result");
	  free_len = (len < mem_info.RegionSize) ? len : mem_info.RegionSize;
	  if (!VirtualFree(start_addr, free_len, MEM_DECOMMIT))
	      ABORT("VirtualFree failed");
	  GC_unmapped_bytes += free_len;
	  start_addr += free_len;
	  len -= free_len;
      }
#   else
      if (len != 0 && munmap(start_addr, len) != 0) ABORT("munmap failed");
      GC_unmapped_bytes += len;
#   endif
}

#endif /* USE_MUNMAP */

/* Routine for pushing any additional roots.  In THREADS 	*/
/* environment, this is also responsible for marking from 	*/
/* thread stacks. 						*/
#ifndef THREADS
void (*GC_push_other_roots)() = 0;
#else /* THREADS */

# ifdef PCR
PCR_ERes GC_push_thread_stack(PCR_Th_T *t, PCR_Any dummy)
{
    struct PCR_ThCtl_TInfoRep info;
    PCR_ERes result;
    
    info.ti_stkLow = info.ti_stkHi = 0;
    result = PCR_ThCtl_GetInfo(t, &info);
    GC_push_all_stack((ptr_t)(info.ti_stkLow), (ptr_t)(info.ti_stkHi));
    return(result);
}

/* Push the contents of an old object. We treat this as stack	*/
/* data only becasue that makes it robust against mark stack	*/
/* overflow.							*/
PCR_ERes GC_push_old_obj(void *p, size_t size, PCR_Any data)
{
    GC_push_all_stack((ptr_t)p, (ptr_t)p + size);
    return(PCR_ERes_okay);
}


void GC_default_push_other_roots GC_PROTO((void))
{
    /* Traverse data allocated by previous memory managers.		*/
	{
	  extern struct PCR_MM_ProcsRep * GC_old_allocator;
	  
	  if ((*(GC_old_allocator->mmp_enumerate))(PCR_Bool_false,
	  					   GC_push_old_obj, 0)
	      != PCR_ERes_okay) {
	      ABORT("Old object enumeration failed");
	  }
	}
    /* Traverse all thread stacks. */
	if (PCR_ERes_IsErr(
                PCR_ThCtl_ApplyToAllOtherThreads(GC_push_thread_stack,0))
              || PCR_ERes_IsErr(GC_push_thread_stack(PCR_Th_CurrThread(), 0))) {
              ABORT("Thread stack marking failed\n");
	}
}

# endif /* PCR */

# ifdef SRC_M3

# ifdef ALL_INTERIOR_POINTERS
    --> misconfigured
# endif

void GC_push_thread_structures GC_PROTO((void))
{
    /* Not our responsibibility. */
}

extern void ThreadF__ProcessStacks();

void GC_push_thread_stack(start, stop)
word start, stop;
{
   GC_push_all_stack((ptr_t)start, (ptr_t)stop + sizeof(word));
}

/* Push routine with M3 specific calling convention. */
GC_m3_push_root(dummy1, p, dummy2, dummy3)
word *p;
ptr_t dummy1, dummy2;
int dummy3;
{
    word q = *p;
    
    GC_PUSH_ONE_STACK(q, p);
}

/* M3 set equivalent to RTHeap.TracedRefTypes */
typedef struct { int elts[1]; }  RefTypeSet;
RefTypeSet GC_TracedRefTypes = {{0x1}};

void GC_default_push_other_roots GC_PROTO((void))
{
    /* Use the M3 provided routine for finding static roots.	 */
    /* This is a bit dubious, since it presumes no C roots.	 */
    /* We handle the collector roots explicitly in GC_push_roots */
      	RTMain__GlobalMapProc(GC_m3_push_root, 0, GC_TracedRefTypes);
	if (GC_words_allocd > 0) {
	    ThreadF__ProcessStacks(GC_push_thread_stack);
	}
	/* Otherwise this isn't absolutely necessary, and we have	*/
	/* startup ordering problems.					*/
}

# endif /* SRC_M3 */

# if defined(GC_SOLARIS_THREADS) || defined(GC_PTHREADS) || \
     defined(GC_WIN32_THREADS)

extern void GC_push_all_stacks();

void GC_default_push_other_roots GC_PROTO((void))
{
    GC_push_all_stacks();
}

# endif /* GC_SOLARIS_THREADS || GC_PTHREADS */

void (*GC_push_other_roots) GC_PROTO((void)) = GC_default_push_other_roots;

#endif /* THREADS */

/*
 * Routines for accessing dirty  bits on virtual pages.
 * We plan to eventually implement four strategies for doing so:
 * DEFAULT_VDB:	A simple dummy implementation that treats every page
 *		as possibly dirty.  This makes incremental collection
 *		useless, but the implementation is still correct.
 * PCR_VDB:	Use PPCRs virtual dirty bit facility.
 * PROC_VDB:	Use the /proc facility for reading dirty bits.  Only
 *		works under some SVR4 variants.  Even then, it may be
 *		too slow to be entirely satisfactory.  Requires reading
 *		dirty bits for entire address space.  Implementations tend
 *		to assume that the client is a (slow) debugger.
 * MPROTECT_VDB:Protect pages and then catch the faults to keep track of
 *		dirtied pages.  The implementation (and implementability)
 *		is highly system dependent.  This usually fails when system
 *		calls write to a protected page.  We prevent the read system
 *		call from doing so.  It is the clients responsibility to
 *		make sure that other system calls are similarly protected
 *		or write only to the stack.
 */
 
GC_bool GC_dirty_maintained = FALSE;

# ifdef DEFAULT_VDB

/* All of the following assume the allocation lock is held, and	*/
/* signals are disabled.					*/

/* The client asserts that unallocated pages in the heap are never	*/
/* written.								*/

/* Initialize virtual dirty bit implementation.			*/
void GC_dirty_init()
{
    GC_dirty_maintained = TRUE;
}

/* Retrieve system dirty bits for heap to a local buffer.	*/
/* Restore the systems notion of which pages are dirty.		*/
void GC_read_dirty()
{}

/* Is the HBLKSIZE sized page at h marked dirty in the local buffer?	*/
/* If the actual page size is different, this returns TRUE if any	*/
/* of the pages overlapping h are dirty.  This routine may err on the	*/
/* side of labelling pages as dirty (and this implementation does).	*/
/*ARGSUSED*/
GC_bool GC_page_was_dirty(h)
struct hblk *h;
{
    return(TRUE);
}

/*
 * The following two routines are typically less crucial.  They matter
 * most with large dynamic libraries, or if we can't accurately identify
 * stacks, e.g. under Solaris 2.X.  Otherwise the following default
 * versions are adequate.
 */
 
/* Could any valid GC heap pointer ever have been written to this page?	*/
/*ARGSUSED*/
GC_bool GC_page_was_ever_dirty(h)
struct hblk *h;
{
    return(TRUE);
}

/* Reset the n pages starting at h to "was never dirty" status.	*/
void GC_is_fresh(h, n)
struct hblk *h;
word n;
{
}

/* A call that:						*/
/* I) hints that [h, h+nblocks) is about to be written.	*/
/* II) guarantees that protection is removed.		*/
/* (I) may speed up some dirty bit implementations.	*/
/* (II) may be essential if we need to ensure that	*/
/* pointer-free system call buffers in the heap are 	*/
/* not protected.					*/
/*ARGSUSED*/
void GC_remove_protection(h, nblocks, is_ptrfree)
struct hblk *h;
word nblocks;
GC_bool is_ptrfree;
{
}

# endif /* DEFAULT_VDB */


# ifdef MPROTECT_VDB

/*
 * See DEFAULT_VDB for interface descriptions.
 */

/*
 * This implementation maintains dirty bits itself by catching write
 * faults and keeping track of them.  We assume nobody else catches
 * SIGBUS or SIGSEGV.  We assume no write faults occur in system calls
 * except as a result of a read system call.  This means clients must
 * either ensure that system calls do not touch the heap, or must
 * provide their own wrappers analogous to the one for read.
 * We assume the page size is a multiple of HBLKSIZE.
 * This implementation is currently SunOS 4.X and IRIX 5.X specific, though we
 * tried to use portable code where easily possible.  It is known
 * not to work under a number of other systems.
 */

# if !defined(MSWIN32) && !defined(MSWINCE)

#   include <sys/mman.h>
#   include <signal.h>
#   include <sys/syscall.h>

#   define PROTECT(addr, len) \
    	  if (mprotect((caddr_t)(addr), (size_t)(len), \
    	      	       PROT_READ | OPT_PROT_EXEC) < 0) { \
    	    ABORT("mprotect failed"); \
    	  }
#   define UNPROTECT(addr, len) \
    	  if (mprotect((caddr_t)(addr), (size_t)(len), \
    	  	       PROT_WRITE | PROT_READ | OPT_PROT_EXEC ) < 0) { \
    	    ABORT("un-mprotect failed"); \
    	  }
    	  
# else

#   ifndef MSWINCE
#     include <signal.h>
#   endif

    static DWORD protect_junk;
#   define PROTECT(addr, len) \
	  if (!VirtualProtect((addr), (len), PAGE_EXECUTE_READ, \
	  		      &protect_junk)) { \
	    DWORD last_error = GetLastError(); \
	    GC_printf1("Last error code: %lx\n", last_error); \
	    ABORT("VirtualProtect failed"); \
	  }
#   define UNPROTECT(addr, len) \
	  if (!VirtualProtect((addr), (len), PAGE_EXECUTE_READWRITE, \
	  		      &protect_junk)) { \
	    ABORT("un-VirtualProtect failed"); \
	  }
	  
# endif

#if defined(SUNOS4) || defined(FREEBSD)
    typedef void (* SIG_PF)();
#endif
#if defined(SUNOS5SIGS) || defined(OSF1) || defined(LINUX) \
    || defined(MACOSX) || defined(HURD)
# ifdef __STDC__
    typedef void (* SIG_PF)(int);
# else
    typedef void (* SIG_PF)();
# endif
#endif
#if defined(MSWIN32)
    typedef LPTOP_LEVEL_EXCEPTION_FILTER SIG_PF;
#   undef SIG_DFL
#   define SIG_DFL (LPTOP_LEVEL_EXCEPTION_FILTER) (-1)
#endif
#if defined(MSWINCE)
    typedef LONG (WINAPI *SIG_PF)(struct _EXCEPTION_POINTERS *);
#   undef SIG_DFL
#   define SIG_DFL (SIG_PF) (-1)
#endif

#if defined(IRIX5) || defined(OSF1) || defined(HURD)
    typedef void (* REAL_SIG_PF)(int, int, struct sigcontext *);
#endif
#if defined(SUNOS5SIGS)
# ifdef HPUX
#   define SIGINFO __siginfo
# else
#   define SIGINFO siginfo
# endif
# ifdef __STDC__
    typedef void (* REAL_SIG_PF)(int, struct SIGINFO *, void *);
# else
    typedef void (* REAL_SIG_PF)();
# endif
#endif
#if defined(LINUX)
#   if __GLIBC__ > 2 || __GLIBC__ == 2 && __GLIBC_MINOR__ >= 2
      typedef struct sigcontext s_c;
#   else  /* glibc < 2.2 */
#     include <linux/version.h>
#     if (LINUX_VERSION_CODE >= 0x20100) && !defined(M68K) || defined(ALPHA)
        typedef struct sigcontext s_c;
#     else
        typedef struct sigcontext_struct s_c;
#     endif
#   endif  /* glibc < 2.2 */
#   if defined(ALPHA) || defined(M68K)
      typedef void (* REAL_SIG_PF)(int, int, s_c *);
#   else
#     if defined(IA64) || defined(HP_PA)
        typedef void (* REAL_SIG_PF)(int, siginfo_t *, s_c *);
#     else
        typedef void (* REAL_SIG_PF)(int, s_c);
#     endif
#   endif
#   ifdef ALPHA
    /* Retrieve fault address from sigcontext structure by decoding	*/
    /* instruction.							*/
    char * get_fault_addr(s_c *sc) {
        unsigned instr;
	word faultaddr;

	instr = *((unsigned *)(sc->sc_pc));
	faultaddr = sc->sc_regs[(instr >> 16) & 0x1f];
	faultaddr += (word) (((int)instr << 16) >> 16);
	return (char *)faultaddr;
    }
#   endif /* !ALPHA */
# endif

# if defined(MACOSX) /* Should also test for PowerPC? */
    typedef void (* REAL_SIG_PF)(int, int, struct sigcontext *);

/* Decodes the machine instruction which was responsible for the sending of the
   SIGBUS signal. Sadly this is the only way to find the faulting address because
   the signal handler doesn't get it directly from the kernel (although it is
   available on the Mach level, but droppped by the BSD personality before it
   calls our signal handler...)
   This code should be able to deal correctly with all PPCs starting from the
   601 up to and including the G4s (including Velocity Engine). */
#define EXTRACT_OP1(iw)     (((iw) & 0xFC000000) >> 26)
#define EXTRACT_OP2(iw)     (((iw) & 0x000007FE) >> 1)
#define EXTRACT_REGA(iw)    (((iw) & 0x001F0000) >> 16)
#define EXTRACT_REGB(iw)    (((iw) & 0x03E00000) >> 21)
#define EXTRACT_REGC(iw)    (((iw) & 0x0000F800) >> 11)
#define EXTRACT_DISP(iw)    ((short *) &(iw))[1]

static char *get_fault_addr(struct sigcontext *scp)
{
   unsigned int   instr = *((unsigned int *) scp->sc_ir);
   unsigned int * regs = &((unsigned int *) scp->sc_regs)[2];
   int            disp = 0, tmp;
   unsigned int   baseA = 0, baseB = 0;
   unsigned int   addr, alignmask = 0xFFFFFFFF;

#ifdef GC_DEBUG_DECODER
   GC_err_printf1("Instruction: 0x%lx\n", instr);
   GC_err_printf1("Opcode 1: d\n", (int)EXTRACT_OP1(instr));
#endif
   switch(EXTRACT_OP1(instr)) {
      case 38:   /* stb */
      case 39:   /* stbu */
      case 54:   /* stfd */
      case 55:   /* stfdu */
      case 52:   /* stfs */
      case 53:   /* stfsu */
      case 44:   /* sth */
      case 45:   /* sthu */
      case 47:   /* stmw */
      case 36:   /* stw */
      case 37:   /* stwu */
            tmp = EXTRACT_REGA(instr);
            if(tmp > 0)
               baseA = regs[tmp];
            disp = EXTRACT_DISP(instr);
            break;
      case 31:
#ifdef GC_DEBUG_DECODER
            GC_err_printf1("Opcode 2: %d\n", (int)EXTRACT_OP2(instr));
#endif
            switch(EXTRACT_OP2(instr)) {
               case 86:    /* dcbf */
               case 54:    /* dcbst */
               case 1014:  /* dcbz */
               case 247:   /* stbux */
               case 215:   /* stbx */
               case 759:   /* stfdux */
               case 727:   /* stfdx */
               case 983:   /* stfiwx */
               case 695:   /* stfsux */
               case 663:   /* stfsx */
               case 918:   /* sthbrx */
               case 439:   /* sthux */
               case 407:   /* sthx */
               case 661:   /* stswx */
               case 662:   /* stwbrx */
               case 150:   /* stwcx. */
               case 183:   /* stwux */
               case 151:   /* stwx */
               case 135:   /* stvebx */
               case 167:   /* stvehx */
               case 199:   /* stvewx */
               case 231:   /* stvx */
               case 487:   /* stvxl */
                     tmp = EXTRACT_REGA(instr);
                     if(tmp > 0)
                        baseA = regs[tmp];
                        baseB = regs[EXTRACT_REGC(instr)];
                        /* determine Altivec alignment mask */
                        switch(EXTRACT_OP2(instr)) {
                           case 167:   /* stvehx */
                                 alignmask = 0xFFFFFFFE;
                                 break;
                           case 199:   /* stvewx */
                                 alignmask = 0xFFFFFFFC;
                                 break;
                           case 231:   /* stvx */
                                 alignmask = 0xFFFFFFF0;
                                 break;
                           case 487:  /* stvxl */
                                 alignmask = 0xFFFFFFF0;
                                 break;
                        }
                        break;
               case 725:   /* stswi */
                     tmp = EXTRACT_REGA(instr);
                     if(tmp > 0)
                        baseA = regs[tmp];
                        break;
               default:   /* ignore instruction */
#ifdef GC_DEBUG_DECODER
                     GC_err_printf("Ignored by inner handler\n");
#endif
                     return NULL;
                    break;
            }
            break;
      default:   /* ignore instruction */
#ifdef GC_DEBUG_DECODER
            GC_err_printf("Ignored by main handler\n");
#endif
            return NULL;
            break;
   }
	
   addr = (baseA + baseB) + disp;
  addr &= alignmask;
#ifdef GC_DEBUG_DECODER
   GC_err_printf1("BaseA: %d\n", baseA);
   GC_err_printf1("BaseB: %d\n", baseB);
   GC_err_printf1("Disp:  %d\n", disp);
   GC_err_printf1("Address: %d\n", addr);
#endif
   return (char *)addr;
}
#endif /* MACOSX */

SIG_PF GC_old_bus_handler;
SIG_PF GC_old_segv_handler;	/* Also old MSWIN32 ACCESS_VIOLATION filter */

#ifdef THREADS
/* We need to lock around the bitmap update in the write fault handler	*/
/* in order to avoid the risk of losing a bit.  We do this with a 	*/
/* test-and-set spin lock if we know how to do that.  Otherwise we	*/
/* check whether we are already in the handler and use the dumb but	*/
/* safe fallback algorithm of setting all bits in the word.		*/
/* Contention should be very rare, so we do the minimum to handle it	*/
/* correctly.								*/
#ifdef GC_TEST_AND_SET_DEFINED
  static VOLATILE unsigned int fault_handler_lock = 0;
  void async_set_pht_entry_from_index(VOLATILE page_hash_table db, int index) {
    while (GC_test_and_set(&fault_handler_lock)) {}
    /* Could also revert to set_pht_entry_from_index_safe if initial	*/
    /* GC_test_and_set fails.						*/
    set_pht_entry_from_index(db, index);
    GC_clear(&fault_handler_lock);
  }
#else /* !GC_TEST_AND_SET_DEFINED */
  /* THIS IS INCORRECT! The dirty bit vector may be temporarily wrong,	*/
  /* just before we notice the conflict and correct it. We may end up   */
  /* looking at it while it's wrong.  But this requires contention	*/
  /* exactly when a GC is triggered, which seems far less likely to	*/
  /* fail than the old code, which had no reported failures.  Thus we	*/
  /* leave it this way while we think of something better, or support	*/
  /* GC_test_and_set on the remaining platforms.			*/
  static VOLATILE word currently_updating = 0;
  void async_set_pht_entry_from_index(VOLATILE page_hash_table db, int index) {
    unsigned int update_dummy;
    currently_updating = (word)(&update_dummy);
    set_pht_entry_from_index(db, index);
    /* If we get contention in the 10 or so instruction window here,	*/
    /* and we get stopped by a GC between the two updates, we lose!	*/
    if (currently_updating != (word)(&update_dummy)) {
	set_pht_entry_from_index_safe(db, index);
	/* We claim that if two threads concurrently try to update the	*/
	/* dirty bit vector, the first one to execute UPDATE_START 	*/
	/* will see it changed when UPDATE_END is executed.  (Note that	*/
	/* &update_dummy must differ in two distinct threads.)  It	*/
	/* will then execute set_pht_entry_from_index_safe, thus 	*/
	/* returning us to a safe state, though not soon enough.	*/
    }
  }
#endif /* !GC_TEST_AND_SET_DEFINED */
#else /* !THREADS */
# define async_set_pht_entry_from_index(db, index) \
	set_pht_entry_from_index(db, index)
#endif /* !THREADS */

/*ARGSUSED*/
# if defined (SUNOS4) || defined(FREEBSD)
    void GC_write_fault_handler(sig, code, scp, addr)
    int sig, code;
    struct sigcontext *scp;
    char * addr;
#   ifdef SUNOS4
#     define SIG_OK (sig == SIGSEGV || sig == SIGBUS)
#     define CODE_OK (FC_CODE(code) == FC_PROT \
              	    || (FC_CODE(code) == FC_OBJERR \
              	       && FC_ERRNO(code) == FC_PROT))
#   endif
#   ifdef FREEBSD
#     define SIG_OK (sig == SIGBUS)
#     define CODE_OK (code == BUS_PAGE_FAULT)
#   endif
# endif
# if defined(IRIX5) || defined(OSF1) || defined(HURD)
#   include <errno.h>
    void GC_write_fault_handler(int sig, int code, struct sigcontext *scp)
#   ifdef OSF1
#     define SIG_OK (sig == SIGSEGV)
#     define CODE_OK (code == 2 /* experimentally determined */)
#   endif
#   ifdef IRIX5
#     define SIG_OK (sig == SIGSEGV)
#     define CODE_OK (code == EACCES)
#   endif
#   ifdef HURD
#     define SIG_OK (sig == SIGBUS || sig == SIGSEGV) 	
#     define CODE_OK  TRUE
#   endif
# endif
# if defined(LINUX)
#   if defined(ALPHA) || defined(M68K)
      void GC_write_fault_handler(int sig, int code, s_c * sc)
#   else
#     if defined(IA64) || defined(HP_PA)
        void GC_write_fault_handler(int sig, siginfo_t * si, s_c * scp)
#     else
        void GC_write_fault_handler(int sig, s_c sc)
#     endif
#   endif
#   define SIG_OK (sig == SIGSEGV)
#   define CODE_OK TRUE
	/* Empirically c.trapno == 14, on IA32, but is that useful?     */
	/* Should probably consider alignment issues on other 		*/
	/* architectures.						*/
# endif
# if defined(SUNOS5SIGS)
#  ifdef __STDC__
    void GC_write_fault_handler(int sig, struct SIGINFO *scp, void * context)
#  else
    void GC_write_fault_handler(sig, scp, context)
    int sig;
    struct SIGINFO *scp;
    void * context;
#  endif
#   ifdef HPUX
#     define SIG_OK (sig == SIGSEGV || sig == SIGBUS)
#     define CODE_OK (scp -> si_code == SEGV_ACCERR) \
		     || (scp -> si_code == BUS_ADRERR) \
		     || (scp -> si_code == BUS_UNKNOWN) \
		     || (scp -> si_code == SEGV_UNKNOWN) \
		     || (scp -> si_code == BUS_OBJERR)
#   else
#     define SIG_OK (sig == SIGSEGV)
#     define CODE_OK (scp -> si_code == SEGV_ACCERR)
#   endif
# endif

# if defined(MACOSX)
    void GC_write_fault_handler(int sig, int code, struct sigcontext *scp)
#   define SIG_OK (sig == SIGBUS)
#   define CODE_OK (code == 0 /* experimentally determined */)
# endif

# if defined(MSWIN32) || defined(MSWINCE)
    LONG WINAPI GC_write_fault_handler(struct _EXCEPTION_POINTERS *exc_info)
#   define SIG_OK (exc_info -> ExceptionRecord -> ExceptionCode == \
			STATUS_ACCESS_VIOLATION)
#   define CODE_OK (exc_info -> ExceptionRecord -> ExceptionInformation[0] == 1)
			/* Write fault */
# endif
{
    register unsigned i;
#   if defined(HURD) 
	char *addr = (char *) code;
#   endif
#   ifdef IRIX5
	char * addr = (char *) (size_t) (scp -> sc_badvaddr);
#   endif
#   if defined(OSF1) && defined(ALPHA)
	char * addr = (char *) (scp -> sc_traparg_a0);
#   endif
#   ifdef SUNOS5SIGS
	char * addr = (char *) (scp -> si_addr);
#   endif
#   ifdef LINUX
#     if defined(I386) || defined (X86_64)
	char * addr = (char *) (sc.cr2);
#     else
#	if defined(M68K)
          char * addr = NULL;

	  struct sigcontext *scp = (struct sigcontext *)(sc);

	  int format = (scp->sc_formatvec >> 12) & 0xf;
	  unsigned long *framedata = (unsigned long *)(scp + 1); 
	  unsigned long ea;

	  if (format == 0xa || format == 0xb) {
	  	/* 68020/030 */
	  	ea = framedata[2];
	  } else if (format == 7) {
	  	/* 68040 */
	  	ea = framedata[3];
	  	if (framedata[1] & 0x08000000) {
	  		/* correct addr on misaligned access */
	  		ea = (ea+4095)&(~4095);
		}
	  } else if (format == 4) {
	  	/* 68060 */
	  	ea = framedata[0];
	  	if (framedata[1] & 0x08000000) {
	  		/* correct addr on misaligned access */
	  		ea = (ea+4095)&(~4095);
	  	}
	  }	
	  addr = (char *)ea;
#	else
#	  ifdef ALPHA
            char * addr = get_fault_addr(sc);
#	  else
#	    if defined(IA64) || defined(HP_PA)
	      char * addr = si -> si_addr;
	      /* I believe this is claimed to work on all platforms for	*/
	      /* Linux 2.3.47 and later.  Hopefully we don't have to	*/
	      /* worry about earlier kernels on IA64.			*/
#	    else
#             if defined(POWERPC)
                char * addr = (char *) (sc.regs->dar);
#	      else
		--> architecture not supported
#	      endif
#	    endif
#	  endif
#	endif
#     endif
#   endif
#   if defined(MACOSX)
        char * addr = get_fault_addr(scp);
#   endif
#   if defined(MSWIN32) || defined(MSWINCE)
	char * addr = (char *) (exc_info -> ExceptionRecord
				-> ExceptionInformation[1]);
#	define sig SIGSEGV
#   endif
    
    if (SIG_OK && CODE_OK) {
        register struct hblk * h =
        		(struct hblk *)((word)addr & ~(GC_page_size-1));
        GC_bool in_allocd_block;
        
#	ifdef SUNOS5SIGS
	    /* Address is only within the correct physical page.	*/
	    in_allocd_block = FALSE;
            for (i = 0; i < divHBLKSZ(GC_page_size); i++) {
              if (HDR(h+i) != 0) {
                in_allocd_block = TRUE;
              }
            }
#	else
	    in_allocd_block = (HDR(addr) != 0);
#	endif
        if (!in_allocd_block) {
	    /* Heap blocks now begin and end on page boundaries */
            SIG_PF old_handler;
            
            if (sig == SIGSEGV) {
            	old_handler = GC_old_segv_handler;
            } else {
                old_handler = GC_old_bus_handler;
            }
            if (old_handler == SIG_DFL) {
#		if !defined(MSWIN32) && !defined(MSWINCE)
		    GC_err_printf1("Segfault at 0x%lx\n", addr);
                    ABORT("Unexpected bus error or segmentation fault");
#		else
		    return(EXCEPTION_CONTINUE_SEARCH);
#		endif
            } else {
#		if defined (SUNOS4) || defined(FREEBSD)
		    (*old_handler) (sig, code, scp, addr);
		    return;
#		endif
#		if defined (SUNOS5SIGS)
		    (*(REAL_SIG_PF)old_handler) (sig, scp, context);
		    return;
#		endif
#		if defined (LINUX)
#		    if defined(ALPHA) || defined(M68K)
		        (*(REAL_SIG_PF)old_handler) (sig, code, sc);
#		    else 
#		      if defined(IA64) || defined(HP_PA)
		        (*(REAL_SIG_PF)old_handler) (sig, si, scp);
#		      else
		        (*(REAL_SIG_PF)old_handler) (sig, sc);
#		      endif
#		    endif
		    return;
#		endif
#		if defined (IRIX5) || defined(OSF1) || defined(HURD)
		    (*(REAL_SIG_PF)old_handler) (sig, code, scp);
		    return;
#		endif
#		ifdef MACOSX
		    (*(REAL_SIG_PF)old_handler) (sig, code, scp);
#		endif
#		ifdef MSWIN32
		    return((*old_handler)(exc_info));
#		endif
            }
        }
        UNPROTECT(h, GC_page_size);
	/* We need to make sure that no collection occurs between	*/
	/* the UNPROTECT and the setting of the dirty bit.  Otherwise	*/
	/* a write by a third thread might go unnoticed.  Reversing	*/
	/* the order is just as bad, since we would end up unprotecting	*/
	/* a page in a GC cycle during which it's not marked.		*/
	/* Currently we do this by disabling the thread stopping	*/
	/* signals while this handler is running.  An alternative might	*/
	/* be to record the fact that we're about to unprotect, or	*/
	/* have just unprotected a page in the GC's thread structure,	*/
	/* and then to have the thread stopping code set the dirty	*/
	/* flag, if necessary.						*/
        for (i = 0; i < divHBLKSZ(GC_page_size); i++) {
            register int index = PHT_HASH(h+i);
            
            async_set_pht_entry_from_index(GC_dirty_pages, index);
        }
#	if defined(OSF1)
	    /* These reset the signal handler each time by default. */
	    signal(SIGSEGV, (SIG_PF) GC_write_fault_handler);
#	endif
    	/* The write may not take place before dirty bits are read.	*/
    	/* But then we'll fault again ...				*/
#	if defined(MSWIN32) || defined(MSWINCE)
	    return(EXCEPTION_CONTINUE_EXECUTION);
#	else
	    return;
#	endif
    }
#if defined(MSWIN32) || defined(MSWINCE)
    return EXCEPTION_CONTINUE_SEARCH;
#else
    GC_err_printf1("Segfault at 0x%lx\n", addr);
    ABORT("Unexpected bus error or segmentation fault");
#endif
}

/*
 * We hold the allocation lock.  We expect block h to be written
 * shortly.  Ensure that all pages cvontaining any part of the n hblks
 * starting at h are no longer protected.  If is_ptrfree is false,
 * also ensure that they will subsequently appear to be dirty.
 */
void GC_remove_protection(h, nblocks, is_ptrfree)
struct hblk *h;
word nblocks;
GC_bool is_ptrfree;
{
    struct hblk * h_trunc;  /* Truncated to page boundary */
    struct hblk * h_end;    /* Page boundary following block end */
    struct hblk * current;
    GC_bool found_clean;
    
    if (!GC_dirty_maintained) return;
    h_trunc = (struct hblk *)((word)h & ~(GC_page_size-1));
    h_end = (struct hblk *)(((word)(h + nblocks) + GC_page_size-1)
	                    & ~(GC_page_size-1));
    found_clean = FALSE;
    for (current = h_trunc; current < h_end; ++current) {
        int index = PHT_HASH(current);
            
        if (!is_ptrfree || current < h || current >= h + nblocks) {
            async_set_pht_entry_from_index(GC_dirty_pages, index);
        }
    }
    UNPROTECT(h_trunc, (ptr_t)h_end - (ptr_t)h_trunc);
}

void GC_dirty_init()
{
#   if defined(SUNOS5SIGS) || defined(IRIX5) || defined(LINUX) || \
       defined(OSF1) || defined(HURD)
      struct sigaction	act, oldact;
      /* We should probably specify SA_SIGINFO for Linux, and handle 	*/
      /* the different architectures more uniformly.			*/
#     if defined(IRIX5) || defined(LINUX) || defined(OSF1) || defined(HURD)
    	act.sa_flags	= SA_RESTART;
        act.sa_handler  = (SIG_PF)GC_write_fault_handler;
#     else
    	act.sa_flags	= SA_RESTART | SA_SIGINFO;
        act.sa_sigaction = GC_write_fault_handler;
#     endif
      (void)sigemptyset(&act.sa_mask);
#     ifdef SIG_SUSPEND
        /* Arrange to postpone SIG_SUSPEND while we're in a write fault	*/
        /* handler.  This effectively makes the handler atomic w.r.t.	*/
        /* stopping the world for GC.					*/
        (void)sigaddset(&act.sa_mask, SIG_SUSPEND);
#     endif /* SIG_SUSPEND */
#    endif
#   if defined(MACOSX)
      struct sigaction act, oldact;

      act.sa_flags = SA_RESTART;
      act.sa_handler = GC_write_fault_handler;
      sigemptyset(&act.sa_mask);
#   endif
#   ifdef PRINTSTATS
	GC_printf0("Inititalizing mprotect virtual dirty bit implementation\n");
#   endif
    GC_dirty_maintained = TRUE;
    if (GC_page_size % HBLKSIZE != 0) {
        GC_err_printf0("Page size not multiple of HBLKSIZE\n");
        ABORT("Page size not multiple of HBLKSIZE");
    }
#   if defined(SUNOS4) || defined(FREEBSD)
      GC_old_bus_handler = signal(SIGBUS, GC_write_fault_handler);
      if (GC_old_bus_handler == SIG_IGN) {
        GC_err_printf0("Previously ignored bus error!?");
        GC_old_bus_handler = SIG_DFL;
      }
      if (GC_old_bus_handler != SIG_DFL) {
#	ifdef PRINTSTATS
          GC_err_printf0("Replaced other SIGBUS handler\n");
#	endif
      }
#   endif
#   if defined(SUNOS4)
      GC_old_segv_handler = signal(SIGSEGV, (SIG_PF)GC_write_fault_handler);
      if (GC_old_segv_handler == SIG_IGN) {
        GC_err_printf0("Previously ignored segmentation violation!?");
        GC_old_segv_handler = SIG_DFL;
      }
      if (GC_old_segv_handler != SIG_DFL) {
#	ifdef PRINTSTATS
          GC_err_printf0("Replaced other SIGSEGV handler\n");
#	endif
      }
#   endif
#   if defined(SUNOS5SIGS) || defined(IRIX5) || defined(LINUX) \
       || defined(OSF1) || defined(HURD)
      /* SUNOS5SIGS includes HPUX */
#     if defined(GC_IRIX_THREADS)
      	sigaction(SIGSEGV, 0, &oldact);
      	sigaction(SIGSEGV, &act, 0);
#     else
      	sigaction(SIGSEGV, &act, &oldact);
#     endif
#     if defined(_sigargs) || defined(HURD)
	/* This is Irix 5.x, not 6.x.  Irix 5.x does not have	*/
	/* sa_sigaction.					*/
	GC_old_segv_handler = oldact.sa_handler;
#     else /* Irix 6.x or SUNOS5SIGS or LINUX */
        if (oldact.sa_flags & SA_SIGINFO) {
          GC_old_segv_handler = (SIG_PF)(oldact.sa_sigaction);
        } else {
          GC_old_segv_handler = oldact.sa_handler;
        }
#     endif
      if (GC_old_segv_handler == SIG_IGN) {
	     GC_err_printf0("Previously ignored segmentation violation!?");
	     GC_old_segv_handler = SIG_DFL;
      }
      if (GC_old_segv_handler != SIG_DFL) {
#       ifdef PRINTSTATS
	  GC_err_printf0("Replaced other SIGSEGV handler\n");
#       endif
      }
#   endif
#   if defined(MACOSX) || defined(HPUX) || defined(LINUX) || defined(HURD)
      sigaction(SIGBUS, &act, &oldact);
      GC_old_bus_handler = oldact.sa_handler;
      if (GC_old_bus_handler == SIG_IGN) {
	     GC_err_printf0("Previously ignored bus error!?");
	     GC_old_bus_handler = SIG_DFL;
      }
      if (GC_old_bus_handler != SIG_DFL) {
#       ifdef PRINTSTATS
	  GC_err_printf0("Replaced other SIGBUS handler\n");
#       endif
      }
#   endif /* MACOS || HPUX || LINUX */
#   if defined(MSWIN32)
      GC_old_segv_handler = SetUnhandledExceptionFilter(GC_write_fault_handler);
      if (GC_old_segv_handler != NULL) {
#	ifdef PRINTSTATS
          GC_err_printf0("Replaced other UnhandledExceptionFilter\n");
#	endif
      } else {
          GC_old_segv_handler = SIG_DFL;
      }
#   endif
}

int GC_incremental_protection_needs()
{
    if (GC_page_size == HBLKSIZE) {
	return GC_PROTECTS_POINTER_HEAP;
    } else {
	return GC_PROTECTS_POINTER_HEAP | GC_PROTECTS_PTRFREE_HEAP;
    }
}

#define HAVE_INCREMENTAL_PROTECTION_NEEDS

#define IS_PTRFREE(hhdr) ((hhdr)->hb_descr == 0)

#define PAGE_ALIGNED(x) !((word)(x) & (GC_page_size - 1))
void GC_protect_heap()
{
    ptr_t start;
    word len;
    struct hblk * current;
    struct hblk * current_start;  /* Start of block to be protected. */
    struct hblk * limit;
    unsigned i;
    GC_bool protect_all = 
	  (0 != (GC_incremental_protection_needs() & GC_PROTECTS_PTRFREE_HEAP));
    for (i = 0; i < GC_n_heap_sects; i++) {
        start = GC_heap_sects[i].hs_start;
        len = GC_heap_sects[i].hs_bytes;
	if (protect_all) {
          PROTECT(start, len);
	} else {
	  GC_ASSERT(PAGE_ALIGNED(len))
	  GC_ASSERT(PAGE_ALIGNED(start))
	  current_start = current = (struct hblk *)start;
	  limit = (struct hblk *)(start + len);
	  while (current < limit) {
            hdr * hhdr;
	    word nhblks;
	    GC_bool is_ptrfree;

	    GC_ASSERT(PAGE_ALIGNED(current));
	    GET_HDR(current, hhdr);
	    if (IS_FORWARDING_ADDR_OR_NIL(hhdr)) {
	      /* This can happen only if we're at the beginning of a 	*/
	      /* heap segment, and a block spans heap segments.		*/
	      /* We will handle that block as part of the preceding	*/
	      /* segment.						*/
	      GC_ASSERT(current_start == current);
	      current_start = ++current;
	      continue;
	    }
	    if (HBLK_IS_FREE(hhdr)) {
	      GC_ASSERT(PAGE_ALIGNED(hhdr -> hb_sz));
	      nhblks = divHBLKSZ(hhdr -> hb_sz);
	      is_ptrfree = TRUE;	/* dirty on alloc */
	    } else {
	      nhblks = OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz);
	      is_ptrfree = IS_PTRFREE(hhdr);
	    }
	    if (is_ptrfree) {
	      if (current_start < current) {
		PROTECT(current_start, (ptr_t)current - (ptr_t)current_start);
	      }
	      current_start = (current += nhblks);
	    } else {
	      current += nhblks;
	    }
	  } 
	  if (current_start < current) {
	    PROTECT(current_start, (ptr_t)current - (ptr_t)current_start);
	  }
	}
    }
}

/* We assume that either the world is stopped or its OK to lose dirty	*/
/* bits while this is happenning (as in GC_enable_incremental).		*/
void GC_read_dirty()
{
    BCOPY((word *)GC_dirty_pages, GC_grungy_pages,
          (sizeof GC_dirty_pages));
    BZERO((word *)GC_dirty_pages, (sizeof GC_dirty_pages));
    GC_protect_heap();
}

GC_bool GC_page_was_dirty(h)
struct hblk * h;
{
    register word index = PHT_HASH(h);
    
    return(HDR(h) == 0 || get_pht_entry_from_index(GC_grungy_pages, index));
}

/*
 * Acquiring the allocation lock here is dangerous, since this
 * can be called from within GC_call_with_alloc_lock, and the cord
 * package does so.  On systems that allow nested lock acquisition, this
 * happens to work.
 * On other systems, SET_LOCK_HOLDER and friends must be suitably defined.
 */

static GC_bool syscall_acquired_lock = FALSE;	/* Protected by GC lock. */
 
void GC_begin_syscall()
{
    if (!I_HOLD_LOCK()) {
	LOCK();
	syscall_acquired_lock = TRUE;
    }
}

void GC_end_syscall()
{
    if (syscall_acquired_lock) {
	syscall_acquired_lock = FALSE;
	UNLOCK();
    }
}

void GC_unprotect_range(addr, len)
ptr_t addr;
word len;
{
    struct hblk * start_block;
    struct hblk * end_block;
    register struct hblk *h;
    ptr_t obj_start;
    
    if (!GC_dirty_maintained) return;
    obj_start = GC_base(addr);
    if (obj_start == 0) return;
    if (GC_base(addr + len - 1) != obj_start) {
        ABORT("GC_unprotect_range(range bigger than object)");
    }
    start_block = (struct hblk *)((word)addr & ~(GC_page_size - 1));
    end_block = (struct hblk *)((word)(addr + len - 1) & ~(GC_page_size - 1));
    end_block += GC_page_size/HBLKSIZE - 1;
    for (h = start_block; h <= end_block; h++) {
        register word index = PHT_HASH(h);
        
        async_set_pht_entry_from_index(GC_dirty_pages, index);
    }
    UNPROTECT(start_block,
    	      ((ptr_t)end_block - (ptr_t)start_block) + HBLKSIZE);
}

#if !defined(MSWIN32) && !defined(MSWINCE) && !defined(THREADS) \
    && !defined(GC_USE_LD_WRAP)
/* Replacement for UNIX system call.					 */
/* Other calls that write to the heap should be handled similarly.	 */
/* Note that this doesn't work well for blocking reads:  It will hold	 */
/* tha allocation lock for the entur duration of the call. Multithreaded */
/* clients should really ensure that it won't block, either by setting 	 */
/* the descriptor nonblocking, or by calling select or poll first, to	 */
/* make sure that input is available.					 */
# if defined(__STDC__) && !defined(SUNOS4)
#   include <unistd.h>
#   include <sys/uio.h>
    ssize_t read(int fd, void *buf, size_t nbyte)
# else
#   ifndef LINT
      int read(fd, buf, nbyte)
#   else
      int GC_read(fd, buf, nbyte)
#   endif
    int fd;
    char *buf;
    int nbyte;
# endif
{
    int result;
    
    GC_begin_syscall();
    GC_unprotect_range(buf, (word)nbyte);
#   if defined(IRIX5) || defined(GC_LINUX_THREADS)
	/* Indirect system call may not always be easily available.	*/
	/* We could call _read, but that would interfere with the	*/
	/* libpthread interception of read.				*/
	/* On Linux, we have to be careful with the linuxthreads	*/
	/* read interception.						*/
	{
	    struct iovec iov;

	    iov.iov_base = buf;
	    iov.iov_len = nbyte;
	    result = readv(fd, &iov, 1);
	}
#   else
#     if defined(HURD)	
	result = __read(fd, buf, nbyte);
#     else
 	/* The two zero args at the end of this list are because one
 	   IA-64 syscall() implementation actually requires six args
 	   to be passed, even though they aren't always used. */
     	result = syscall(SYS_read, fd, buf, nbyte, 0, 0);
#     endif /* !HURD */
#   endif
    GC_end_syscall();
    return(result);
}
#endif /* !MSWIN32 && !MSWINCE && !GC_LINUX_THREADS */

#if defined(GC_USE_LD_WRAP) && !defined(THREADS)
    /* We use the GNU ld call wrapping facility.			*/
    /* This requires that the linker be invoked with "--wrap read".	*/
    /* This can be done by passing -Wl,"--wrap read" to gcc.		*/
    /* I'm not sure that this actually wraps whatever version of read	*/
    /* is called by stdio.  That code also mentions __read.		*/
#   include <unistd.h>
    ssize_t __wrap_read(int fd, void *buf, size_t nbyte)
    {
 	int result;

	GC_begin_syscall();
    	GC_unprotect_range(buf, (word)nbyte);
	result = __real_read(fd, buf, nbyte);
	GC_end_syscall();
	return(result);
    }

    /* We should probably also do this for __read, or whatever stdio	*/
    /* actually calls.							*/
#endif

/*ARGSUSED*/
GC_bool GC_page_was_ever_dirty(h)
struct hblk *h;
{
    return(TRUE);
}

/* Reset the n pages starting at h to "was never dirty" status.	*/
/*ARGSUSED*/
void GC_is_fresh(h, n)
struct hblk *h;
word n;
{
}

# else /* !MPROTECT_VDB */

#   ifdef GC_USE_LD_WRAP
      ssize_t __wrap_read(int fd, void *buf, size_t nbyte)
      { return __real_read(fd, buf, nbyte); }
#   endif

# endif /* MPROTECT_VDB */

# ifdef PROC_VDB

/*
 * See DEFAULT_VDB for interface descriptions.
 */
 
/*
 * This implementaion assumes a Solaris 2.X like /proc pseudo-file-system
 * from which we can read page modified bits.  This facility is far from
 * optimal (e.g. we would like to get the info for only some of the
 * address space), but it avoids intercepting system calls.
 */

#include <errno.h>
#include <sys/types.h>
#include <sys/signal.h>
#include <sys/fault.h>
#include <sys/syscall.h>
#include <sys/procfs.h>
#include <sys/stat.h>

#define INITIAL_BUF_SZ 4096
word GC_proc_buf_size = INITIAL_BUF_SZ;
char *GC_proc_buf;

#ifdef GC_SOLARIS_THREADS
/* We don't have exact sp values for threads.  So we count on	*/
/* occasionally declaring stack pages to be fresh.  Thus we 	*/
/* need a real implementation of GC_is_fresh.  We can't clear	*/
/* entries in GC_written_pages, since that would declare all	*/
/* pages with the given hash address to be fresh.		*/
#   define MAX_FRESH_PAGES 8*1024	/* Must be power of 2 */
    struct hblk ** GC_fresh_pages;	/* A direct mapped cache.	*/
    					/* Collisions are dropped.	*/

#   define FRESH_PAGE_SLOT(h) (divHBLKSZ((word)(h)) & (MAX_FRESH_PAGES-1))
#   define ADD_FRESH_PAGE(h) \
	GC_fresh_pages[FRESH_PAGE_SLOT(h)] = (h)
#   define PAGE_IS_FRESH(h) \
	(GC_fresh_pages[FRESH_PAGE_SLOT(h)] == (h) && (h) != 0)
#endif

/* Add all pages in pht2 to pht1 */
void GC_or_pages(pht1, pht2)
page_hash_table pht1, pht2;
{
    register int i;
    
    for (i = 0; i < PHT_SIZE; i++) pht1[i] |= pht2[i];
}

int GC_proc_fd;

void GC_dirty_init()
{
    int fd;
    char buf[30];

    GC_dirty_maintained = TRUE;
    if (GC_words_allocd != 0 || GC_words_allocd_before_gc != 0) {
    	register int i;
    
        for (i = 0; i < PHT_SIZE; i++) GC_written_pages[i] = (word)(-1);
#       ifdef PRINTSTATS
	    GC_printf1("Allocated words:%lu:all pages may have been written\n",
	    	       (unsigned long)
	    	      		(GC_words_allocd + GC_words_allocd_before_gc));
#	endif       
    }
    sprintf(buf, "/proc/%d", getpid());
    fd = open(buf, O_RDONLY);
    if (fd < 0) {
    	ABORT("/proc open failed");
    }
    GC_proc_fd = syscall(SYS_ioctl, fd, PIOCOPENPD, 0);
    close(fd);
    if (GC_proc_fd < 0) {
    	ABORT("/proc ioctl failed");
    }
    GC_proc_buf = GC_scratch_alloc(GC_proc_buf_size);
#   ifdef GC_SOLARIS_THREADS
	GC_fresh_pages = (struct hblk **)
	  GC_scratch_alloc(MAX_FRESH_PAGES * sizeof (struct hblk *));
	if (GC_fresh_pages == 0) {
	    GC_err_printf0("No space for fresh pages\n");
	    EXIT();
	}
	BZERO(GC_fresh_pages, MAX_FRESH_PAGES * sizeof (struct hblk *));
#   endif
}

/* Ignore write hints. They don't help us here.	*/
/*ARGSUSED*/
void GC_remove_protection(h, nblocks, is_ptrfree)
struct hblk *h;
word nblocks;
GC_bool is_ptrfree;
{
}

#ifdef GC_SOLARIS_THREADS
#   define READ(fd,buf,nbytes) syscall(SYS_read, fd, buf, nbytes)
#else
#   define READ(fd,buf,nbytes) read(fd, buf, nbytes)
#endif

void GC_read_dirty()
{
    unsigned long ps, np;
    int nmaps;
    ptr_t vaddr;
    struct prasmap * map;
    char * bufp;
    ptr_t current_addr, limit;
    int i;
int dummy;

    BZERO(GC_grungy_pages, (sizeof GC_grungy_pages));
    
    bufp = GC_proc_buf;
    if (READ(GC_proc_fd, bufp, GC_proc_buf_size) <= 0) {
#	ifdef PRINTSTATS
            GC_printf1("/proc read failed: GC_proc_buf_size = %lu\n",
            	       GC_proc_buf_size);
#	endif       
        {
            /* Retry with larger buffer. */
            word new_size = 2 * GC_proc_buf_size;
            char * new_buf = GC_scratch_alloc(new_size);
            
            if (new_buf != 0) {
                GC_proc_buf = bufp = new_buf;
                GC_proc_buf_size = new_size;
            }
            if (syscall(SYS_read, GC_proc_fd, bufp, GC_proc_buf_size) <= 0) {
                WARN("Insufficient space for /proc read\n", 0);
                /* Punt:	*/
        	memset(GC_grungy_pages, 0xff, sizeof (page_hash_table));
		memset(GC_written_pages, 0xff, sizeof(page_hash_table));
#		ifdef GC_SOLARIS_THREADS
		    BZERO(GC_fresh_pages,
		    	  MAX_FRESH_PAGES * sizeof (struct hblk *)); 
#		endif
		return;
            }
        }
    }
    /* Copy dirty bits into GC_grungy_pages */
    	nmaps = ((struct prpageheader *)bufp) -> pr_nmap;
	/* printf( "nmaps = %d, PG_REFERENCED = %d, PG_MODIFIED = %d\n",
		     nmaps, PG_REFERENCED, PG_MODIFIED); */
	bufp = bufp + sizeof(struct prpageheader);
	for (i = 0; i < nmaps; i++) {
	    map = (struct prasmap *)bufp;
	    vaddr = (ptr_t)(map -> pr_vaddr);
	    ps = map -> pr_pagesize;
	    np = map -> pr_npage;
	    /* printf("vaddr = 0x%X, ps = 0x%X, np = 0x%X\n", vaddr, ps, np); */
	    limit = vaddr + ps * np;
	    bufp += sizeof (struct prasmap);
	    for (current_addr = vaddr;
	         current_addr < limit; current_addr += ps){
	        if ((*bufp++) & PG_MODIFIED) {
	            register struct hblk * h = (struct hblk *) current_addr;
	            
	            while ((ptr_t)h < current_addr + ps) {
	                register word index = PHT_HASH(h);
	                
	                set_pht_entry_from_index(GC_grungy_pages, index);
#			ifdef GC_SOLARIS_THREADS
			  {
			    register int slot = FRESH_PAGE_SLOT(h);
			    
			    if (GC_fresh_pages[slot] == h) {
			        GC_fresh_pages[slot] = 0;
			    }
			  }
#			endif
	                h++;
	            }
	        }
	    }
	    bufp += sizeof(long) - 1;
	    bufp = (char *)((unsigned long)bufp & ~(sizeof(long)-1));
	}
    /* Update GC_written_pages. */
        GC_or_pages(GC_written_pages, GC_grungy_pages);
#   ifdef GC_SOLARIS_THREADS
      /* Make sure that old stacks are considered completely clean	*/
      /* unless written again.						*/
	GC_old_stacks_are_fresh();
#   endif
}

#undef READ

GC_bool GC_page_was_dirty(h)
struct hblk *h;
{
    register word index = PHT_HASH(h);
    register GC_bool result;
    
    result = get_pht_entry_from_index(GC_grungy_pages, index);
#   ifdef GC_SOLARIS_THREADS
	if (result && PAGE_IS_FRESH(h)) result = FALSE;
	/* This happens only if page was declared fresh since	*/
	/* the read_dirty call, e.g. because it's in an unused  */
	/* thread stack.  It's OK to treat it as clean, in	*/
	/* that case.  And it's consistent with 		*/
	/* GC_page_was_ever_dirty.				*/
#   endif
    return(result);
}

GC_bool GC_page_was_ever_dirty(h)
struct hblk *h;
{
    register word index = PHT_HASH(h);
    register GC_bool result;
    
    result = get_pht_entry_from_index(GC_written_pages, index);
#   ifdef GC_SOLARIS_THREADS
	if (result && PAGE_IS_FRESH(h)) result = FALSE;
#   endif
    return(result);
}

/* Caller holds allocation lock.	*/
void GC_is_fresh(h, n)
struct hblk *h;
word n;
{

    register word index;
    
#   ifdef GC_SOLARIS_THREADS
      register word i;
      
      if (GC_fresh_pages != 0) {
        for (i = 0; i < n; i++) {
          ADD_FRESH_PAGE(h + i);
        }
      }
#   endif
}

# endif /* PROC_VDB */


# ifdef PCR_VDB

# include "vd/PCR_VD.h"

# define NPAGES (32*1024)	/* 128 MB */

PCR_VD_DB  GC_grungy_bits[NPAGES];

ptr_t GC_vd_base;	/* Address corresponding to GC_grungy_bits[0]	*/
			/* HBLKSIZE aligned.				*/

void GC_dirty_init()
{
    GC_dirty_maintained = TRUE;
    /* For the time being, we assume the heap generally grows up */
    GC_vd_base = GC_heap_sects[0].hs_start;
    if (GC_vd_base == 0) {
   	ABORT("Bad initial heap segment");
    }
    if (PCR_VD_Start(HBLKSIZE, GC_vd_base, NPAGES*HBLKSIZE)
	!= PCR_ERes_okay) {
	ABORT("dirty bit initialization failed");
    }
}

void GC_read_dirty()
{
    /* lazily enable dirty bits on newly added heap sects */
    {
        static int onhs = 0;
        int nhs = GC_n_heap_sects;
        for( ; onhs < nhs; onhs++ ) {
            PCR_VD_WriteProtectEnable(
                    GC_heap_sects[onhs].hs_start,
                    GC_heap_sects[onhs].hs_bytes );
        }
    }


    if (PCR_VD_Clear(GC_vd_base, NPAGES*HBLKSIZE, GC_grungy_bits)
        != PCR_ERes_okay) {
	ABORT("dirty bit read failed");
    }
}

GC_bool GC_page_was_dirty(h)
struct hblk *h;
{
    if((ptr_t)h < GC_vd_base || (ptr_t)h >= GC_vd_base + NPAGES*HBLKSIZE) {
	return(TRUE);
    }
    return(GC_grungy_bits[h - (struct hblk *)GC_vd_base] & PCR_VD_DB_dirtyBit);
}

/*ARGSUSED*/
void GC_remove_protection(h, nblocks, is_ptrfree)
struct hblk *h;
word nblocks;
GC_bool is_ptrfree;
{
    PCR_VD_WriteProtectDisable(h, nblocks*HBLKSIZE);
    PCR_VD_WriteProtectEnable(h, nblocks*HBLKSIZE);
}

# endif /* PCR_VDB */

# ifndef HAVE_INCREMENTAL_PROTECTION_NEEDS
  int GC_incremental_protection_needs()
  {
    return GC_PROTECTS_NONE;
  }
# endif /* !HAVE_INCREMENTAL_PROTECTION_NEEDS */

/*
 * Call stack save code for debugging.
 * Should probably be in mach_dep.c, but that requires reorganization.
 */

/* I suspect the following works for most X86 *nix variants, so 	*/
/* long as the frame pointer is explicitly stored.  In the case of gcc,	*/
/* compiler flags (e.g. -fomit-frame-pointer) determine whether it is.	*/
#if defined(I386) && defined(LINUX) && defined(SAVE_CALL_CHAIN)
#   include <features.h>

    struct frame {
	struct frame *fr_savfp;
	long	fr_savpc;
        long	fr_arg[NARGS];  /* All the arguments go here.	*/
    };
#endif

#if defined(SPARC)
#  if defined(LINUX)
#    include <features.h>

     struct frame {
	long	fr_local[8];
	long	fr_arg[6];
	struct frame *fr_savfp;
	long	fr_savpc;
#       ifndef __arch64__
	  char	*fr_stret;
#       endif
	long	fr_argd[6];
	long	fr_argx[0];
     };
#  else
#    if defined(SUNOS4)
#      include <machine/frame.h>
#    else
#      if defined (DRSNX)
#	 include <sys/sparc/frame.h>
#      else
#	 if defined(OPENBSD) || defined(NETBSD)
#	   include <frame.h>
#	 else
#	   include <sys/frame.h>
#	 endif
#      endif
#    endif
#  endif
#  if NARGS > 6
	--> We only know how to to get the first 6 arguments
#  endif
#endif /* SPARC */

#ifdef SAVE_CALL_CHAIN
/* Fill in the pc and argument information for up to NFRAMES of my	*/
/* callers.  Ignore my frame and my callers frame.			*/

#ifdef LINUX
# include <features.h>
# if __GLIBC__ == 2 && __GLIBC_MINOR__ >= 1 || __GLIBC__ > 2
#   define HAVE_BUILTIN_BACKTRACE
# endif
#endif

#if NARGS == 0 && NFRAMES % 2 == 0 /* No padding */ \
    && defined(HAVE_BUILTIN_BACKTRACE)

#include <execinfo.h>

void GC_save_callers (info) 
struct callinfo info[NFRAMES];
{
  void * tmp_info[NFRAMES + 1];
  int npcs, i;
# define IGNORE_FRAMES 1
  
  /* We retrieve NFRAMES+1 pc values, but discard the first, since it	*/
  /* points to our own frame.						*/
  GC_ASSERT(sizeof(struct callinfo) == sizeof(void *));
  npcs = backtrace((void **)tmp_info, NFRAMES + IGNORE_FRAMES);
  BCOPY(tmp_info+IGNORE_FRAMES, info, (npcs - IGNORE_FRAMES) * sizeof(void *));
  for (i = npcs - IGNORE_FRAMES; i < NFRAMES; ++i) info[i].ci_pc = 0;
}

#else /* No builtin backtrace; do it ourselves */

#if (defined(OPENBSD) || defined(NETBSD)) && defined(SPARC)
#  define FR_SAVFP fr_fp
#  define FR_SAVPC fr_pc
#else
#  define FR_SAVFP fr_savfp
#  define FR_SAVPC fr_savpc
#endif

#if defined(SPARC) && (defined(__arch64__) || defined(__sparcv9))
#   define BIAS 2047
#else
#   define BIAS 0
#endif

void GC_save_callers (info) 
struct callinfo info[NFRAMES];
{
  struct frame *frame;
  struct frame *fp;
  int nframes = 0;
# ifdef I386
    /* We assume this is turned on only with gcc as the compiler. */
    asm("movl %%ebp,%0" : "=r"(frame));
    fp = frame;
# else
    word GC_save_regs_in_stack();

    frame = (struct frame *) GC_save_regs_in_stack ();
    fp = (struct frame *)((long) frame -> FR_SAVFP + BIAS);
#endif
  
   for (; (!(fp HOTTER_THAN frame) && !(GC_stackbottom HOTTER_THAN (ptr_t)fp)
	   && (nframes < NFRAMES));
       fp = (struct frame *)((long) fp -> FR_SAVFP + BIAS), nframes++) {
      register int i;
      
      info[nframes].ci_pc = fp->FR_SAVPC;
#     if NARGS > 0
        for (i = 0; i < NARGS; i++) {
	  info[nframes].ci_arg[i] = ~(fp->fr_arg[i]);
        }
#     endif /* NARGS > 0 */
  }
  if (nframes < NFRAMES) info[nframes].ci_pc = 0;
}

#endif /* No builtin backtrace */

#endif /* SAVE_CALL_CHAIN */

#if defined(LINUX) && defined(__ELF__) && \
    (!defined(SMALL_CONFIG) || defined(USE_PROC_FOR_LIBRARIES))
#ifdef GC_USE_LD_WRAP
#   define READ __real_read
#else
#   define READ read
#endif


/* Repeatedly perform a read call until the buffer is filled or	*/
/* we encounter EOF.						*/
ssize_t GC_repeat_read(int fd, char *buf, size_t count)
{
    ssize_t num_read = 0;
    ssize_t result;
    
    while (num_read < count) {
	result = READ(fd, buf + num_read, count - num_read);
	if (result < 0) return result;
	if (result == 0) break;
	num_read += result;
    }
    return num_read;
}
#endif /* LINUX && ... */


#if defined(LINUX) && defined(__ELF__) && !defined(SMALL_CONFIG)

/* Dump /proc/self/maps to GC_stderr, to enable looking up names for
   addresses in FIND_LEAK output. */

void GC_print_address_map()
{
    int f;
    int result;
    char maps_temp[32768];
    GC_err_printf0("---------- Begin address map ----------\n");
        f = open("/proc/self/maps", O_RDONLY);
        if (-1 == f) ABORT("Couldn't open /proc/self/maps");
	do {
	    result = GC_repeat_read(f, maps_temp, sizeof(maps_temp));
	    if (result <= 0) ABORT("Couldn't read /proc/self/maps");
 	    GC_err_write(maps_temp, result);
	} while (result == sizeof(maps_temp));
     
    GC_err_printf0("---------- End address map ----------\n");
}

#endif


