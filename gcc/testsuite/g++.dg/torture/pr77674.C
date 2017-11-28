// { dg-do compile }
typedef struct { } __fsid_t;
typedef unsigned long int pthread_t;
extern "C" {
  extern __inline __attribute__ ((__gnu_inline__)) int pthread_equal (pthread_t __thread1, pthread_t __thread2) throw ()   {
      return 0;
  }
}
typedef pthread_t __gthread_t;
static __typeof (pthread_equal)      __gthrw_pthread_equal __attribute__ ((__weakref__ ("pthread_equal")));

static inline int __gthread_equal (__gthread_t __t1, __gthread_t __t2)
{
  return __gthrw_pthread_equal (__t1, __t2);
}

