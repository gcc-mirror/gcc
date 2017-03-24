/* Make sure the vector load and zero instruction is being used for
   initializing a 32 bit vector with the first element taken from
   memory.  */

/* { dg-do run } */
/* { dg-options "-O3 -mzarch -march=arch12 --save-temps" } */
/* { dg-require-effective-target s390_vxe } */

typedef unsigned int       uv4si __attribute__((vector_size(16)));

uv4si __attribute__((noinline))
foo (int *a)
{
  return (uv4si){ *a, 0, 0, 0 };
}

int
main ()
{
  int b = 4;
  uv4si a = (uv4si){ 1, 2, 3, 4 };

  a = foo (&b);

  if (a[0] != 4 || a[1] != 0 || a[2] != 0 || a[3] != 0)
    __builtin_abort ();

  return 0;
}
/* { dg-final { scan-assembler-times "vllezlf\t%v24,0\\(%r2\\)" 1 } } */
