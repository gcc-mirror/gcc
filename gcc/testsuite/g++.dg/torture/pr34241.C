/* { dg-do compile } */

inline void *operator  new (__SIZE_TYPE__, void *__p) throw ()
{
  return __p;
}
struct A
{
  A(int, double);
  inline explicit A (int pattern, bool cs)
  {
    new (this) A (pattern, double(cs));
  }
};
A test ()
{
  const A a (42, true);
  return a;
}
