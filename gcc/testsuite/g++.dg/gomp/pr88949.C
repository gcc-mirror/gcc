// PR c++/88949
// { dg-do compile }

struct A {
  int a;
  A (int x) : a (x) {
#pragma omp parallel firstprivate (a)
    --a;
  }
  void foo () {
#pragma omp parallel firstprivate (a)
    --a;
  }
};

int c;

int
main ()
{
  A d(c);
  d.foo ();
}
