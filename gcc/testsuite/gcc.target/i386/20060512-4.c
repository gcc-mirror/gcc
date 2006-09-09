/* { dg-do compile { target i?86-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-mstackrealign" } */
int
outer_function (int x, int y)
{
  int __attribute__ ((__noinline__))
  nested_function (int x, int y)
    { /* { dg-error "-mstackrealign ignored for nested functions" } */
      return (x + y);
    }
  return (3 + nested_function (x, y));
}
