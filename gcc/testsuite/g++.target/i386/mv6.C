/* Test to check if member version multiversioning works correctly.  */

/* { dg-do run } */
/* { dg-require-ifunc "" }  */
/* { dg-options "-march=x86-64" } */

class Foo
{
 public:
  /* Default version of foo.  */
  __attribute__ ((target("default")))
  int foo ()
  {
    return 0;
  }
  /* corei7 version of foo.  */
  __attribute__ ((target("arch=corei7")))
  int foo ()
  {
    return 0;
  }
};

int main ()
{
  Foo f;
  return f.foo ();
}
