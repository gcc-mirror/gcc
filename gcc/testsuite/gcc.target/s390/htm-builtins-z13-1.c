/* Verify if VRs are saved and restored.  */

/* { dg-do run } */
/* { dg-require-effective-target s390_vx } */
/* { dg-options "-O3 -march=z13 -mzarch" } */

typedef int __attribute__((vector_size(16))) v4si;

v4si __attribute__((noinline))
foo (v4si a)
{
  a += (v4si){ 1, 1, 1, 1 };
  if (__builtin_tbegin (0) == 0)
    {
      a += (v4si){ 1, 1, 1, 1 };
      __builtin_tabort (256);
      __builtin_tend ();
    }
  else
    a -= (v4si){ 1, 1, 1, 1 };

  return a;
}

int
main ()
{
  v4si a = (v4si){ 0, 0, 0, 0 };

  a = foo (a);

  if (a[0] != 0)
    __builtin_abort ();
}
