/* { dg-do compile { target i?86-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-std=gnu99" } */
int
outer_function (int x, int y)
{
  int __attribute__ ((__noinline__, __force_align_arg_pointer__))
  nested_function (int x, int y)
    { /* { dg-error "force_align_arg_pointer not supported for nested functions" } */
      return (x + y);
    }
  return (3 + nested_function (x, y));
}
