// { dg-do compile }
static __typeof 0 a __attribute__ ((__weakref__ ("")));
template <typename> class A
{
  static __thread int b;
};
