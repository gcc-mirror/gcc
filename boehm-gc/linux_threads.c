/* 
 * Copyright (c) 1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1998 by Fergus Henderson.  All rights reserved.
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
/*
 * Support code for LinuxThreads, the clone()-based kernel
 * thread package for Linux which is included in libc6.
 *
 * This code relies on implementation details of LinuxThreads,
 * (i.e. properties not guaranteed by the Pthread standard):
 *
 *	- the function GC_linux_thread_top_of_stack(void)
 *	  relies on the way LinuxThreads lays out thread stacks
 *	  in the address space.
 *
 * Note that there is a lot of code duplication between linux_threads.c
 * and irix_threads.c; any changes made here may need to be reflected
 * there too.
 */

/* #define DEBUG_THREADS 1 */

/* ANSI C requires that a compilation unit contains something */
# include "gc_priv.h"

# if defined(LINUX_THREADS)

# include <pthread.h>
# include <sched.h>
# include <time.h>
# include <errno.h>
# include <unistd.h>
# include <sys/mman.h>
# include <sys/time.h>
# include <semaphore.h>
# include <signal.h>

#ifdef USE_LD_WRAP
#   define WRAP_FUNC(f) __wrap_##f
#   define REAL_FUNC(f) __real_##f
#else
#   define WRAP_FUNC(f) GC_##f
#   define REAL_FUNC(f) f
#   undef pthread_create
#   undef pthread_sigmask
#   undef pthread_join
#endif


void GC_thr_init();

#if 0
void GC_print_sig_mask()
{
    sigset_t blocked;
    int i;

    if (pthread_sigmask(SIG_BLOCK, NULL, &blocked) != 0)
    	ABORT("pthread_sigmask");
    GC_printf0("Blocked: ");
    for (i = 1; i <= MAXSIG; i++) {
        if (sigismember(&blocked, i)) { GC_printf1("%ld ",(long) i); }
    }
    GC_printf0("\n");
}
#endif

/* We use the allocation lock to protect thread-related data structures. */

/* The set of all known threads.  We intercept thread creation and 	*/
/* joins.  We never actually create detached threads.  We allocate all 	*/
/* new thread stacks ourselves.  These allow us to maintain this	*/
/* data structure.							*/
/* Protected by GC_thr_lock.						*/
/* Some of this should be declared volatile, but that's incosnsistent	*/
/* with some library routine declarations.  		 		*/
typedef struct GC_Thread_Rep {
    struct GC_Thread_Rep * next;  /* More recently allocated threads	*/
				  /* with a given pthread id come 	*/
				  /* first.  (All but the first are	*/
				  /* guaranteed to be dead, but we may  */
				  /* not yet have registered the join.) */
    pthread_t id;
    word flags;
#	define FINISHED 1   	/* Thread has exited.	*/
#	define DETACHED 2	/* Thread is intended to be detached.	*/
#	define MAIN_THREAD 4	/* True for the original thread only.	*/

    ptr_t stack_end;		/* Cold end of the stack.		*/
    ptr_t stack_ptr;  		/* Valid only when stopped.      	*/
#   ifdef IA64
	ptr_t backing_store_end;
	ptr_t backing_store_ptr;
#   endif
    int	signal;
    void * status;		/* The value returned from the thread.  */
    				/* Used only to avoid premature 	*/
				/* reclamation of any data it might 	*/
				/* reference.				*/
} * GC_thread;

GC_thread GC_lookup_thread(pthread_t id);

/*
 * The only way to suspend threads given the pthread interface is to send
 * signals.  We can't use SIGSTOP directly, because we need to get the
 * thread to save its stack pointer in the GC thread table before
 * suspending.  So we have to reserve a signal of our own for this.
 * This means we have to intercept client calls to change the signal mask.
 * The linuxthreads package already uses SIGUSR1 and SIGUSR2,
 * so we need to reuse something else.  I chose SIGPWR.
 * (Perhaps SIGUNUSED would be a better choice.)
 */
#define SIG_SUSPEND SIGPWR

#define SIG_RESTART SIGXCPU

sem_t GC_suspend_ack_sem;

/*
GC_linux_thread_top_of_stack() relies on implementation details of
LinuxThreads, namely that thread stacks are allocated on 2M boundaries
and grow to no more than 2M.
To make sure that we're using LinuxThreads and not some other thread
package, we generate a dummy reference to `pthread_kill_other_threads_np'
(was `__pthread_initial_thread_bos' but that disappeared),
which is a symbol defined in LinuxThreads, but (hopefully) not in other
thread packages.
*/
void (*dummy_var_to_force_linux_threads)() = pthread_kill_other_threads_np;

#define LINUX_THREADS_STACK_SIZE  (2 * 1024 * 1024)

static inline ptr_t GC_linux_thread_top_of_stack(void)
{
  char *sp = GC_approx_sp();
  ptr_t tos = (ptr_t) (((unsigned long)sp | (LINUX_THREADS_STACK_SIZE - 1)) + 1);
#if DEBUG_THREADS
  GC_printf1("SP = %lx\n", (unsigned long)sp);
  GC_printf1("TOS = %lx\n", (unsigned long)tos);
#endif
  return tos;
}

#if defined(SPARC) || defined(IA64)
  extern word GC_save_regs_in_stack();
#endif

void GC_suspend_handler(int sig)
{
    int dummy;
    pthread_t my_thread = pthread_self();
    GC_thread me;
    sigset_t all_sigs;
    sigset_t old_sigs;
    int i;
    sigset_t mask;

    if (sig != SIG_SUSPEND) ABORT("Bad signal in suspend_handler");

#if DEBUG_THREADS
    GC_printf1("Suspending 0x%x\n", my_thread);
#endif

    me = GC_lookup_thread(my_thread);
    /* The lookup here is safe, since I'm doing this on behalf  */
    /* of a thread which holds the allocation lock in order	*/
    /* to stop the world.  Thus concurrent modification of the	*/
    /* data structure is impossible.				*/
#   ifdef SPARC
	me -> stack_ptr = (ptr_t)GC_save_regs_in_stack();
#   else
	me -> stack_ptr = (ptr_t)(&dummy);
#   endif
#   ifdef IA64
	me -> backing_store_ptr = (ptr_t)GC_save_regs_in_stack();
#   endif

    /* Tell the thread that wants to stop the world that this   */
    /* thread has been stopped.  Note that sem_post() is  	*/
    /* the only async-signal-safe primitive in LinuxThreads.    */
    sem_post(&GC_suspend_ack_sem);

    /* Wait until that thread tells us to restart by sending    */
    /* this thread a SIG_RESTART signal.			*/
    /* SIG_RESTART should be masked at this point.  Thus there	*/
    /* is no race.						*/
    if (sigfillset(&mask) != 0) ABORT("sigfillset() failed");
    if (sigdelset(&mask, SIG_RESTART) != 0) ABORT("sigdelset() failed");
#   ifdef NO_SIGNALS
      if (sigdelset(&mask, SIGINT) != 0) ABORT("sigdelset() failed");
      if (sigdelset(&mask, SIGQUIT) != 0) ABORT("sigdelset() failed");
      if (sigdelset(&mask, SIGTERM) != 0) ABORT("sigdelset() failed");
#   endif
    do {
	    me->signal = 0;
	    sigsuspend(&mask);             /* Wait for signal */
    } while (me->signal != SIG_RESTART);

#if DEBUG_THREADS
    GC_printf1("Continuing 0x%x\n", my_thread);
#endif
}

void GC_restart_handler(int sig)
{
    GC_thread me;

    if (sig != SIG_RESTART) ABORT("Bad signal in suspend_handler");

    /* Let the GC_suspend_handler() know that we got a SIG_RESTART. */
    /* The lookup here is safe, since I'm doing this on behalf  */
    /* of a thread which holds the allocation lock in order	*/
    /* to stop the world.  Thus concurrent modification of the	*/
    /* data structure is impossible.				*/
    me = GC_lookup_thread(pthread_self());
    me->signal = SIG_RESTART;

    /*
    ** Note: even if we didn't do anything useful here,
    ** it would still be necessary to have a signal handler,
    ** rather than ignoring the signals, otherwise
    ** the signals will not be delivered at all, and
    ** will thus not interrupt the sigsuspend() above.
    */

#if DEBUG_THREADS
    GC_printf1("In GC_restart_handler for 0x%x\n", pthread_self());
#endif
}

GC_bool GC_thr_initialized = FALSE;

# define THREAD_TABLE_SZ 128	/* Must be power of 2	*/
volatile GC_thread GC_threads[THREAD_TABLE_SZ];

/* Add a thread to GC_threads.  We assume it wasn't already there.	*/
/* Caller holds allocation lock.					*/
GC_thread GC_new_thread(pthread_t id)
{
    int hv = ((word)id) % THREAD_TABLE_SZ;
    GC_thread result;
    static struct GC_Thread_Rep first_thread;
    static GC_bool first_thread_used = FALSE;
    
    if (!first_thread_used) {
    	result = &first_thread;
    	first_thread_used = TRUE;
    	/* Dont acquire allocation lock, since we may already hold it. */
    } else {
        result = (struct GC_Thread_Rep *)
        	 GC_generic_malloc_inner(sizeof(struct GC_Thread_Rep), NORMAL);
    }
    if (result == 0) return(0);
    result -> id = id;
    result -> next = GC_threads[hv];
    GC_threads[hv] = result;
    /* result -> flags = 0; */
    return(result);
}

/* Delete a thread from GC_threads.  We assume it is there.	*/
/* (The code intentionally traps if it wasn't.)			*/
/* Caller holds allocation lock.				*/
void GC_delete_thread(pthread_t id)
{
    int hv = ((word)id) % THREAD_TABLE_SZ;
    register GC_thread p = GC_threads[hv];
    register GC_thread prev = 0;
    
    while (!pthread_equal(p -> id, id)) {
        prev = p;
        p = p -> next;
    }
    if (prev == 0) {
        GC_threads[hv] = p -> next;
    } else {
        prev -> next = p -> next;
    }
}

/* If a thread has been joined, but we have not yet		*/
/* been notified, then there may be more than one thread 	*/
/* in the table with the same pthread id.			*/
/* This is OK, but we need a way to delete a specific one.	*/
void GC_delete_gc_thread(pthread_t id, GC_thread gc_id)
{
    int hv = ((word)id) % THREAD_TABLE_SZ;
    register GC_thread p = GC_threads[hv];
    register GC_thread prev = 0;

    while (p != gc_id) {
        prev = p;
        p = p -> next;
    }
    if (prev == 0) {
        GC_threads[hv] = p -> next;
    } else {
        prev -> next = p -> next;
    }
}

/* Return a GC_thread corresponding to a given thread_t.	*/
/* Returns 0 if it's not there.					*/
/* Caller holds  allocation lock or otherwise inhibits 		*/
/* updates.							*/
/* If there is more than one thread with the given id we 	*/
/* return the most recent one.					*/
GC_thread GC_lookup_thread(pthread_t id)
{
    int hv = ((word)id) % THREAD_TABLE_SZ;
    register GC_thread p = GC_threads[hv];
    
    while (p != 0 && !pthread_equal(p -> id, id)) p = p -> next;
    return(p);
}

/* Caller holds allocation lock.	*/
void GC_stop_world()
{
    pthread_t my_thread = pthread_self();
    register int i;
    register GC_thread p;
    register int n_live_threads = 0;
    register int result;

    for (i = 0; i < THREAD_TABLE_SZ; i++) {
      for (p = GC_threads[i]; p != 0; p = p -> next) {
        if (p -> id != my_thread) {
            if (p -> flags & FINISHED) continue;
            n_live_threads++;
	    #if DEBUG_THREADS
	      GC_printf1("Sending suspend signal to 0x%x\n", p -> id);
	    #endif
            result = pthread_kill(p -> id, SIG_SUSPEND);
	    switch(result) {
                case ESRCH:
                    /* Not really there anymore.  Possible? */
                    n_live_threads--;
                    break;
                case 0:
                    break;
                default:
                    ABORT("pthread_kill failed");
            }
        }
      }
    }
    for (i = 0; i < n_live_threads; i++) {
    	sem_wait(&GC_suspend_ack_sem);
    }
    #if DEBUG_THREADS
    GC_printf1("World stopped 0x%x\n", pthread_self());
    #endif
}

/* Caller holds allocation lock.	*/
void GC_start_world()
{
    pthread_t my_thread = pthread_self();
    register int i;
    register GC_thread p;
    register int n_live_threads = 0;
    register int result;
    
#   if DEBUG_THREADS
      GC_printf0("World starting\n");
#   endif

    for (i = 0; i < THREAD_TABLE_SZ; i++) {
      for (p = GC_threads[i]; p != 0; p = p -> next) {
        if (p -> id != my_thread) {
            if (p -> flags & FINISHED) continue;
            n_live_threads++;
	    #if DEBUG_THREADS
	      GC_printf1("Sending restart signal to 0x%x\n", p -> id);
	    #endif
            result = pthread_kill(p -> id, SIG_RESTART);
	    switch(result) {
                case ESRCH:
                    /* Not really there anymore.  Possible? */
                    n_live_threads--;
                    break;
                case 0:
                    break;
                default:
                    ABORT("pthread_kill failed");
            }
        }
      }
    }
    #if DEBUG_THREADS
      GC_printf0("World started\n");
    #endif
}

# ifdef IA64
#   define IF_IA64(x) x
# else
#   define IF_IA64(x)
# endif
/* We hold allocation lock.  Should do exactly the right thing if the	*/
/* world is stopped.  Should not fail if it isn't.			*/
void GC_push_all_stacks()
{
    int i;
    GC_thread p;
    ptr_t sp = GC_approx_sp();
    ptr_t lo, hi;
    /* On IA64, we also need to scan the register backing store. */
    IF_IA64(ptr_t bs_lo; ptr_t bs_hi;)
    pthread_t me = pthread_self();
    
    if (!GC_thr_initialized) GC_thr_init();
    #if DEBUG_THREADS
        GC_printf1("Pushing stacks from thread 0x%lx\n", (unsigned long) me);
    #endif
    for (i = 0; i < THREAD_TABLE_SZ; i++) {
      for (p = GC_threads[i]; p != 0; p = p -> next) {
        if (p -> flags & FINISHED) continue;
        if (pthread_equal(p -> id, me)) {
#  ifdef SPARC
	    lo = (ptr_t)GC_save_regs_in_stack();
#  else
	    lo = GC_approx_sp();
#  endif
	    IF_IA64(bs_hi = (ptr_t)GC_save_regs_in_stack();)
	} else {
	    lo = p -> stack_ptr;
	    IF_IA64(bs_hi = p -> backing_store_ptr;)
	}
        if ((p -> flags & MAIN_THREAD) == 0) {
	    hi = p -> stack_end;
	    IF_IA64(bs_lo = p -> backing_store_end);
        } else {
            /* The original stack. */
            hi = GC_stackbottom;
	    IF_IA64(bs_lo = BACKING_STORE_BASE;)
        }
        #if DEBUG_THREADS
            GC_printf3("Stack for thread 0x%lx = [%lx,%lx)\n",
    	        (unsigned long) p -> id,
		(unsigned long) lo, (unsigned long) hi);
        #endif
	if (0 == lo) ABORT("GC_push_all_stacks: sp not set!\n");
        GC_push_all_stack(lo, hi);
#	ifdef IA64
          if (pthread_equal(p -> id, me)) {
	    GC_push_all_eager(bs_lo, bs_hi);
	  } else {
	    GC_push_all_stack(bs_lo, bs_hi);
	  }
#	endif
      }
    }
}


/* We hold the allocation lock.	*/
void GC_thr_init()
{
    int dummy;
    GC_thread t;
    struct sigaction act;

    if (GC_thr_initialized) return;
    GC_thr_initialized = TRUE;

    if (sem_init(&GC_suspend_ack_sem, 0, 0) != 0)
    	ABORT("sem_init failed");

    act.sa_flags = SA_RESTART;
    if (sigfillset(&act.sa_mask) != 0) {
    	ABORT("sigfillset() failed");
    }

#   ifdef NO_SIGNALS
      if (sigdelset(&act.sa_mask, SIGINT) != 0
	  || sigdelset(&act.sa_mask, SIGQUIT != 0)
	  || sigdelset(&act.sa_mask, SIGTERM != 0)) {
        ABORT("sigdelset() failed");
      }
#   endif

    /* SIG_RESTART is unmasked by the handler when necessary. 	*/
    act.sa_handler = GC_suspend_handler;
    if (sigaction(SIG_SUSPEND, &act, NULL) != 0) {
    	ABORT("Cannot set SIG_SUSPEND handler");
    }

    act.sa_handler = GC_restart_handler;
    if (sigaction(SIG_RESTART, &act, NULL) != 0) {
    	ABORT("Cannot set SIG_SUSPEND handler");
    }

    /* Add the initial thread, so we can stop it.	*/
      t = GC_new_thread(pthread_self());
      t -> stack_ptr = (ptr_t)(&dummy);
      t -> flags = DETACHED | MAIN_THREAD;
}

int WRAP_FUNC(pthread_sigmask)(int how, const sigset_t *set, sigset_t *oset)
{
    sigset_t fudged_set;
    
    if (set != NULL && (how == SIG_BLOCK || how == SIG_SETMASK)) {
        fudged_set = *set;
        sigdelset(&fudged_set, SIG_SUSPEND);
        set = &fudged_set;
    }
    return(REAL_FUNC(pthread_sigmask)(how, set, oset));
}

struct start_info {
    void *(*start_routine)(void *);
    void *arg;
    word flags;
    sem_t registered;   	/* 1 ==> in our thread table, but 	*/
				/* parent hasn't yet noticed.		*/
};


void GC_thread_exit_proc(void *arg)
{
    GC_thread me;
    struct start_info * si = arg;

    LOCK();
    me = GC_lookup_thread(pthread_self());
    if (me -> flags & DETACHED) {
    	GC_delete_thread(pthread_self());
    } else {
	me -> flags |= FINISHED;
    }
    if (GC_incremental && GC_collection_in_progress()) {
	int old_gc_no = GC_gc_no;

	/* Make sure that no part of our stack is still on the mark stack, */
	/* since it's about to be unmapped.				   */
	while (GC_incremental && GC_collection_in_progress()
	       && old_gc_no == GC_gc_no) {
	    ENTER_GC();
            GC_collect_a_little_inner(1);
	    EXIT_GC();
	    UNLOCK();
	    sched_yield();
	    LOCK();
	}
    }
    UNLOCK();
}

int WRAP_FUNC(pthread_join)(pthread_t thread, void **retval)
{
    int result;
    GC_thread thread_gc_id;
    
    LOCK();
    thread_gc_id = GC_lookup_thread(thread);
    /* This is guaranteed to be the intended one, since the thread id	*/
    /* cant have been recycled by pthreads.				*/
    UNLOCK();
    result = REAL_FUNC(pthread_join)(thread, retval);
    LOCK();
    /* Here the pthread thread id may have been recycled. */
    GC_delete_gc_thread(thread, thread_gc_id);
    UNLOCK();
    return result;
}

void * GC_start_routine(void * arg)
{
    int dummy;
    struct start_info * si = arg;
    void * result;
    GC_thread me;
    pthread_t my_pthread;
    void *(*start)(void *);
    void *start_arg;

    my_pthread = pthread_self();
#   ifdef DEBUG_THREADS
        GC_printf1("Starting thread 0x%lx\n", my_pthread);
        GC_printf1("pid = %ld\n", (long) getpid());
        GC_printf1("sp = 0x%lx\n", (long) &arg);
#   endif
    LOCK();
    me = GC_new_thread(my_pthread);
    me -> flags = si -> flags;
    me -> stack_ptr = 0;
    /* me -> stack_end = GC_linux_stack_base(); -- currently (11/99)	*/
    /* doesn't work because the stack base in /proc/self/stat is the 	*/
    /* one for the main thread.  There is a strong argument that that's	*/
    /* a kernel bug, but a pervasive one.				*/
#   ifdef STACK_GROWS_DOWN
      me -> stack_end = (ptr_t)(((word)(&dummy) + (GC_page_size - 1))
		                & ~(GC_page_size - 1));
      me -> stack_ptr = me -> stack_end - 0x10;
	/* Needs to be plausible, since an asynchronous stack mark	*/
	/* should not crash.						*/
#   else
      me -> stack_end = (ptr_t)(((word)(&dummy) & ~(GC_page_size - 1));
      me -> stack_ptr = me -> stack_end + 0x10;
#   endif
    /* This is dubious, since we may be more than a page into the stack, */
    /* and hence skip some of it, though it's not clear that matters.	 */
#   ifdef IA64
      me -> backing_store_end = (ptr_t)
			(GC_save_regs_in_stack() & ~(GC_page_size - 1));
      /* This is also < 100% convincing.  We should also read this 	*/
      /* from /proc, but the hook to do so isn't there yet.		*/
#   endif /* IA64 */
    UNLOCK();
    start = si -> start_routine;
#   ifdef DEBUG_THREADS
	GC_printf1("start_routine = 0x%lx\n", start);
#   endif
    start_arg = si -> arg;
    sem_post(&(si -> registered));
    pthread_cleanup_push(GC_thread_exit_proc, si);
    result = (*start)(start_arg);
#if DEBUG_THREADS
        GC_printf1("Finishing thread 0x%x\n", pthread_self());
#endif
    me -> status = result;
    me -> flags |= FINISHED;
    pthread_cleanup_pop(1);
    /* Cleanup acquires lock, ensuring that we can't exit		*/
    /* while a collection that thinks we're alive is trying to stop     */
    /* us.								*/
    return(result);
}

int
WRAP_FUNC(pthread_create)(pthread_t *new_thread,
		  const pthread_attr_t *attr,
                  void *(*start_routine)(void *), void *arg)
{
    int result;
    GC_thread t;
    pthread_t my_new_thread;
    void * stack;
    size_t stacksize;
    pthread_attr_t new_attr;
    int detachstate;
    word my_flags = 0;
    struct start_info * si = GC_malloc(sizeof(struct start_info)); 
	/* This is otherwise saved only in an area mmapped by the thread */
	/* library, which isn't visible to the collector.		 */

    if (0 == si) return(ENOMEM);
    sem_init(&(si -> registered), 0, 0);
    si -> start_routine = start_routine;
    si -> arg = arg;
    LOCK();
    if (!GC_thr_initialized) GC_thr_init();
    if (NULL == attr) {
        stack = 0;
	(void) pthread_attr_init(&new_attr);
    } else {
        new_attr = *attr;
    }
    pthread_attr_getdetachstate(&new_attr, &detachstate);
    if (PTHREAD_CREATE_DETACHED == detachstate) my_flags |= DETACHED;
    si -> flags = my_flags;
    UNLOCK();
#   ifdef DEBUG_THREADS
        GC_printf1("About to start new thread from thread 0x%X\n",
		   pthread_self());
#   endif
    result = REAL_FUNC(pthread_create)(new_thread, &new_attr, GC_start_routine, si);
#   ifdef DEBUG_THREADS
        GC_printf1("Started thread 0x%X\n", *new_thread);
#   endif
    /* Wait until child has been added to the thread table.		*/
    /* This also ensures that we hold onto si until the child is done	*/
    /* with it.  Thus it doesn't matter whether it is otherwise		*/
    /* visible to the collector.					*/
        if (0 != sem_wait(&(si -> registered))) ABORT("sem_wait failed");
        sem_destroy(&(si -> registered));
    /* pthread_attr_destroy(&new_attr); */
    /* pthread_attr_destroy(&new_attr); */
    return(result);
}

#if defined(USE_SPIN_LOCK)

VOLATILE GC_bool GC_collecting = 0;
			/* A hint that we're in the collector and       */
                        /* holding the allocation lock for an           */
                        /* extended period.                             */

/* Reasonably fast spin locks.  Basically the same implementation */
/* as STL alloc.h.  This isn't really the right way to do this.   */
/* but until the POSIX scheduling mess gets straightened out ...  */

volatile unsigned int GC_allocate_lock = 0;


void GC_lock()
{
#   define low_spin_max 30  /* spin cycles if we suspect uniprocessor */
#   define high_spin_max 1000 /* spin cycles for multiprocessor */
    static unsigned spin_max = low_spin_max;
    unsigned my_spin_max;
    static unsigned last_spins = 0;
    unsigned my_last_spins;
    volatile unsigned junk;
#   define PAUSE junk *= junk; junk *= junk; junk *= junk; junk *= junk
    int i;

    if (!GC_test_and_set(&GC_allocate_lock)) {
        return;
    }
    junk = 0;
    my_spin_max = spin_max;
    my_last_spins = last_spins;
    for (i = 0; i < my_spin_max; i++) {
        if (GC_collecting) goto yield;
        if (i < my_last_spins/2 || GC_allocate_lock) {
            PAUSE; 
            continue;
        }
        if (!GC_test_and_set(&GC_allocate_lock)) {
	    /*
             * got it!
             * Spinning worked.  Thus we're probably not being scheduled
             * against the other process with which we were contending.
             * Thus it makes sense to spin longer the next time.
	     */
            last_spins = i;
            spin_max = high_spin_max;
            return;
        }
    }
    /* We are probably being scheduled against the other process.  Sleep. */
    spin_max = low_spin_max;
yield:
    for (i = 0;; ++i) {
        if (!GC_test_and_set(&GC_allocate_lock)) {
            return;
        }
#       define SLEEP_THRESHOLD 12
		/* nanosleep(<= 2ms) just spins under Linux.  We	*/
		/* want to be careful to avoid that behavior.		*/
        if (i < SLEEP_THRESHOLD) {
            sched_yield();
	} else {
	    struct timespec ts;
	
	    if (i > 26) i = 26;
			/* Don't wait for more than about 60msecs, even	*/
			/* under extreme contention.			*/
	    ts.tv_sec = 0;
	    ts.tv_nsec = 1 << i;
	    nanosleep(&ts, 0);
	}
    }
}

#endif /* known architecture */

# endif /* LINUX_THREADS */

