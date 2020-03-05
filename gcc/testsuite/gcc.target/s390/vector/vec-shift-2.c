/* { dg-do run } */
/* { dg-options "-O3 -mzarch -march=z13 --save-temps" } */

/* { dg-final { scan-assembler-times "veslf" 1 } } */

typedef __attribute__((vector_size(16))) signed int v4si;

v4si __attribute__((noinline,noclone))
shift_left_by_scalar (v4si in, int shift_count)
{
  return in << (3 + shift_count);
}

int
main ()
{
  v4si a = { 1, 2, 3, 4 };
  v4si result = shift_left_by_scalar (a, 1);

  if (result[1] != 32)
    __builtin_abort ();

  return 0;
}
