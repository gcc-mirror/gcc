/* { dg-do link } */
/* { dg-do run { target runtime_ptx_alias } } */
/* { dg-options "-save-temps" } */
/* { dg-add-options ptx_alias } */

/* Copy of alias-1.c, with static __f and f.  */

int v;

static void __f ()
{
  v = 1;
}

static void f () __attribute__ ((alias ("__f")));

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
/* { dg-final { scan-assembler-times "\\.func __f;" 1 } } */
/* { dg-final { scan-assembler-times "\\.func f;" 1 } } */
