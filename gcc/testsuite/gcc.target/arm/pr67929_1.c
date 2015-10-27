/* { dg-do run } */
/* { dg-require-effective-target arm_vfp3_ok } */
/* { dg-options "-O2 -fno-inline" } */
/* { dg-add-options arm_vfp3 } */
/* { dg-skip-if "need fp instructions" { *-*-* } { "-mfloat-abi=soft" } { "" } } */

int
foo (float a)
{
  return a * 4.9f;
}


int
main (void)
{
  if (foo (10.0f) != 49)
    __builtin_abort ();

  return 0;
}