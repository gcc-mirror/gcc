// { dg-do compile { target c++11 } }
// { dg-require-effective-target tls }
// { dg-options "-O2" }
// { dg-add-options tls }

extern void pthread_key_create();
static __typeof(pthread_key_create) __gthrw___pthread_key_create
    __attribute__((__weakref__("")));
template <class> void zeta_imp_odd_integer() { thread_local int digits; }
