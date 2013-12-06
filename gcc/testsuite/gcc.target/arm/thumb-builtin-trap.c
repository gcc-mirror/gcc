/* { dg-do compile } */
/* { dg-options "-mthumb" } */
/* { dg-require-effective-target arm_thumb1_ok } */

void
trap ()
{
  __builtin_trap ();
}

/* { dg-final { scan-assembler "0xdeff" } } */
