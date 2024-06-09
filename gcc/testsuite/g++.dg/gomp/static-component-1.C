/* { dg-do compile } */

/* Types with static members should be mappable.  */

struct A {
  static int x[10];
};

struct B {
  A a;
};

int
main (int argc, char *argv[])
{
  B *b = new B;
#pragma omp target map(b->a)
  ;
  B bb;
#pragma omp target map(bb.a)
  ;
  delete b;
}
