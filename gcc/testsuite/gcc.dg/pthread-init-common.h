/* 
 * Common code for the pthread-init-*.c tests.
 *
 * Origin: Kaveh Ghazi (ghazi@caip.rutgers.edu) 9/27/2006.
 */

#include <pthread.h>
#define UNUSED __attribute__ ((__unused__))

void foo(void)
{
#ifdef PTHREAD_MUTEX_INITIALIZER
  pthread_mutex_t pmutex UNUSED = PTHREAD_MUTEX_INITIALIZER;
#endif
#ifdef PTHREAD_COND_INITIALIZER
  pthread_cond_t pcond UNUSED = PTHREAD_COND_INITIALIZER;
#endif
#ifdef PTHREAD_RWLOCK_INITIALIZER
  pthread_rwlock_t prwlock UNUSED = PTHREAD_RWLOCK_INITIALIZER;
#endif
#ifdef PTHREAD_ONCE_INIT
  pthread_once_t ponce UNUSED = PTHREAD_ONCE_INIT;
#endif
}
