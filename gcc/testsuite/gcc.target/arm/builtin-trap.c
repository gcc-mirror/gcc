/* { dg-do compile } */
/* { dg-require-effective-target arm32 } */

void
trap ()
{
  __builtin_trap ();
}

/* { dg-final { scan-assembler "0xe7f000f0" { target { arm_nothumb } } } } */
