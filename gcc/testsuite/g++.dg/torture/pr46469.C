extern "C"  __inline __attribute__ ((__gnu_inline__)) int pthread_equal ()
  {
  }

static
  __typeof
  (pthread_equal)
  __gthrw_pthread_equal __attribute__ ((__weakref__ ("pthread_equal")));

int identifierByPthreadHandle ()
{
  pthread_equal ();
}
