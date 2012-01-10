/* PR 51113 */
/* { dg-options "-fprofile-arcs -ftest-coverage -fpic" } */
/* { dg-do run { target native } } */
/* { dg-additional-sources "gcovpart-12b.C" } */

struct Foo {
  Foo ()
  {}  /* count(1) */
  virtual void Func () /* count(#####) */
  {}   /* count(#####) */
};

int main ()
{
  Foo b;
  
  return 0;  /* count (1) */
}

/* { dg-final { run-gcov gcov-12.C } } */
/* { dg-final { run-gcov gcovpart-12b.C { xfail *-*-* } } } */
