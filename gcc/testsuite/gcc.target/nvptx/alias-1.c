/* { dg-do link } */
/* { dg-do run { target runtime_ptx_isa_version_6_3 } } */
/* { dg-options "-save-temps -malias -mptx=6.3" } */

int v;

void __f ()
{
  v = 1;
}

void f () __attribute__ ((alias ("__f")));

int
main (void)
{
  if (v != 0)
    __builtin_abort ();
  f ();
  if (v != 1)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-assembler-times "\\.alias f,__f;" 1 } } */
/* { dg-final { scan-assembler-times "\\.visible \\.func __f;" 1 } } */
/* { dg-final { scan-assembler-times "\\.visible \\.func f;" 1 } } */
