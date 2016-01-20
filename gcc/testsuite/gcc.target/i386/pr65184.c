/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-O2 -mabi=ms -fcheck-pointer-bounds -mmpx" } */

void
foo (int *a)
{
  if (a[0] != a[1] * 2333)
    __builtin_abort ();
}

void
bar (int *a)
{
  if (a[0] != a[1] * 2333)
    __builtin_abort ();
}
