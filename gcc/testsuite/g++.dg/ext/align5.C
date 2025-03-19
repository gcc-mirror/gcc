// PR c++/117512
// { dg-do run }

struct A {
  __attribute__((aligned(2 * sizeof (int)))) int i;
  ~A() {}
};

A foo () { A a = { 19 }; return a; }

int
main ()
{
  A a = { 42 };
  A r = foo () = a;
  if (r.i != 42)
    __builtin_abort ();
}
