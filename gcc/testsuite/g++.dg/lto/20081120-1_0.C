// { dg-lto-do link }
// { dg-lto-options {{-flto -shared}} }
extern "C"
{
 extern __inline __attribute__((__gnu_inline__)) int pthread_equal(int, int)
 {
 }
}
static __typeof(pthread_equal)
    __gthrw_pthread_equal __attribute__((__weakref__("pthread_equal")));
