/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target aarch64_small_fpic } */
/* { dg-options "-O2 -fpic -fno-inline --save-temps" } */
/* { dg-skip-if "-fpic for AArch64 small code model" { aarch64*-*-* }  { "-mcmodel=tiny" "-mcmodel=large" } { "" } } */

void abort ();
int global_a;

int
initialize (void)
{
  global_a = 0x10;
  return global_a - 1;
}

int
main (int argc, char **argv)
{
  int a = initialize ();

  if (a != global_a - 1)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "adrp\tx\[0-9\]+, _GLOBAL_OFFSET_TABLE" 2 } } */
/* { dg-final { cleanup-saved-temps } } */
