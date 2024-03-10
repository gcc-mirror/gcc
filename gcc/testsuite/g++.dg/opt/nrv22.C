// PR c++/92407
// { dg-do run }

struct A
{
  A () { a++; }
  A (const A &) { a++; }
  ~A () { a--; }
  static int a;
};
int A::a = 0;

A
foo ()
{
  int cnt = 10;
lab:
  A a;
  if (cnt--)
    goto lab;
  return a;
}

int
main ()
{
  foo ();
  if (A::a)
    __builtin_abort ();
}
