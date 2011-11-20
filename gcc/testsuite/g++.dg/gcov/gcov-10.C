/* Ensure PIC sequence used for comdat functions */

/* { dg-options "-fprofile-arcs -ftest-coverage -fpic" } */
/* { dg-do run { target native } } */
/* { dg-require-effective-target fpic } */

inline int __attribute__ ((noinline)) Foo ()
{
  static int x[1];

  return x[0]++;  /* count (1) */
}

int main ()
{
  Foo ();  /* count (1) */
  return 0;  /* count (1) */
}

/* { dg-final { run-gcov gcov-10.C } } */
