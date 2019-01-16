// PR rtl-optimization/88478
// { dg-do compile }
// { dg-options "-O2" }

struct A {
  bool b;
  int s;
  template <typename T, typename U>
  A (T, U) {}
};
enum F {} f;

A
foo ()
{
  return A (false, f);
}
