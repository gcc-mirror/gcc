#include "private/pthread_support.h"

# if defined(GC_DARWIN_THREADS)

#define DEBUG_THREADS 0

/* From "Inside Mac OS X - Mach-O Runtime Architecture" published by Apple
   Page 49:
   "The space beneath the stack pointer, where a new stack frame would normally
   be allocated, is called the red zone. This area as shown in Figure 3-2 may
   be used for any purpose as long as a new stack frame does not need to be
   added to the stack."
   
   Page 50: "If a leaf procedure's red zone usage would exceed 224 bytes, then
   it must set up a stack frame just like routines that call other routines."
*/
#define PPC_RED_ZONE_SIZE 224

void GC_push_all_stacks() {
    int i;
    kern_return_t r;
    GC_thread p;
    pthread_t me;
    ptr_t lo, hi;
#	if defined(POWERPC)
        ppc_thread_state_t state;
#	else
#		error FIXME for non-ppc OS X
#	endif
    mach_msg_type_number_t thread_state_count = MACHINE_THREAD_STATE_COUNT;
    
    me = pthread_self();
    if (!GC_thr_initialized) GC_thr_init();
    
    for(i=0;i<THREAD_TABLE_SZ;i++) {
        for(p=GC_threads[i];p!=0;p=p->next) {
            if(p -> flags & FINISHED) continue;
            if(pthread_equal(p->id,me)) {
                lo = GC_approx_sp();
            } else {
                /* Get the thread state (registers, etc) */
                r = thread_get_state(
                    p->stop_info.mach_thread,
                    MACHINE_THREAD_STATE,
                    (natural_t*)&state,
                    &thread_state_count);
                if(r != KERN_SUCCESS) ABORT("thread_get_state failed");
    
                #ifdef POWERPC
                    lo = (void*)(state.r1 - PPC_RED_ZONE_SIZE);
                    
                    GC_push_one(state.r0); 
                    GC_push_one(state.r2); 
                    GC_push_one(state.r3); 
                    GC_push_one(state.r4); 
                    GC_push_one(state.r5); 
                    GC_push_one(state.r6); 
                    GC_push_one(state.r7); 
                    GC_push_one(state.r8); 
                    GC_push_one(state.r9); 
                    GC_push_one(state.r10); 
                    GC_push_one(state.r11); 
                    GC_push_one(state.r12); 
                    GC_push_one(state.r13); 
                    GC_push_one(state.r14); 
                    GC_push_one(state.r15); 
                    GC_push_one(state.r16); 
                    GC_push_one(state.r17); 
                    GC_push_one(state.r18); 
                    GC_push_one(state.r19); 
                    GC_push_one(state.r20); 
                    GC_push_one(state.r21); 
                    GC_push_one(state.r22); 
                    GC_push_one(state.r23); 
                    GC_push_one(state.r24); 
                    GC_push_one(state.r25); 
                    GC_push_one(state.r26); 
                    GC_push_one(state.r27); 
                    GC_push_one(state.r28); 
                    GC_push_one(state.r29); 
                    GC_push_one(state.r30); 
                    GC_push_one(state.r31);
                #else
                #	error FIXME for non-PPC darwin
                #endif /* !POWERPC */
            } /* p != me */
            if(p->flags & MAIN_THREAD)
                hi = GC_stackbottom;
            else
                hi = p->stack_end;
            #if DEBUG_THREADS
                GC_printf3("Darwin: Stack for thread 0x%lx = [%lx,%lx)\n",
                    (unsigned long) p -> id,
                    (unsigned long) lo,
                    (unsigned long) hi
                );
            #endif
            GC_push_all_stack(lo,hi);
        } /* for(p=GC_threads[i]...) */
    } /* for(i=0;i<THREAD_TABLE_SZ...) */
}

/* Caller holds allocation lock.	*/
void GC_stop_world()
{
    int i;
    GC_thread p;
    pthread_t my_thread = pthread_self();
    kern_return_t kern_result;
    
    #if DEBUG_THREADS
    GC_printf1("Stopping the world from 0x%lx\n", pthread_self());
    #endif
       
    /* Make sure all free list construction has stopped before we start. */
    /* No new construction can start, since free list construction is	*/
    /* required to acquire and release the GC lock before it starts,	*/
    /* and we have the lock.						*/
#   ifdef PARALLEL_MARK
      GC_acquire_mark_lock();
      GC_ASSERT(GC_fl_builder_count == 0);
      /* We should have previously waited for it to become zero. */
#   endif /* PARALLEL_MARK */

    for (i = 0; i < THREAD_TABLE_SZ; i++) {
        for (p = GC_threads[i]; p != 0; p = p -> next) {
            if (p -> id == my_thread) continue;
            if (p -> flags & FINISHED) continue;
            if (p -> thread_blocked) /* Will wait */ continue;
            
            #if DEBUG_THREADS
            GC_printf1("Suspending thread 0x%lx\n", p -> id);
            #endif
            
            /* Suspend the thread */
            kern_result = thread_suspend(p->stop_info.mach_thread);
            if(kern_result != KERN_SUCCESS) ABORT("thread_suspend failed");
            
            /* This is only needed if we are modifying the threads 
               state. thread_abort_safely should also be used
               if this code is ever added in again.
               
               kern_result = thread_abort(p->stop_info.mach_thread);
               if(kern_result != KERN_SUCCESS)
                   ABORT("thread_abort failed (%ul)",kern_result);
            */
        }
    }
    
#   ifdef MPROTECT_VDB
    if(GC_incremental) {
        extern void GC_mprotect_stop();
        GC_mprotect_stop();
    }
#   endif
    
#   ifdef PARALLEL_MARK
      GC_release_mark_lock();
#   endif
    #if DEBUG_THREADS
      GC_printf1("World stopped from 0x%lx\n", pthread_self());
    #endif
}

/* Caller holds allocation lock, and has held it continuously since	*/
/* the world stopped.							*/
void GC_start_world()
{
    pthread_t my_thread = pthread_self();
    int i;
    GC_thread p;
    kern_return_t kern_result;

#   if DEBUG_THREADS
      GC_printf0("World starting\n");
#   endif

#   ifdef MPROTECT_VDB
    if(GC_incremental) {
        extern void GC_mprotect_resume();
        GC_mprotect_resume();
    }
#   endif

    for (i = 0; i < THREAD_TABLE_SZ; i++) {
        for (p = GC_threads[i]; p != 0; p = p -> next) {
            if (p -> id == my_thread) continue;
            if (p -> flags & FINISHED) continue;
            if (p -> thread_blocked) continue;
    
            #if DEBUG_THREADS
            GC_printf1("Resuming 0x%lx\n", p -> id);
            #endif
            
            /* Resume the thread */
            kern_result = thread_resume(p->stop_info.mach_thread);
            if(kern_result != KERN_SUCCESS) ABORT("thread_resume failed");
        }
    }
    #if DEBUG_THREADS
      GC_printf0("World started\n");
    #endif
}

void GC_stop_init() {

}

#endif
