/* { dg-do compile { target pie_enabled } } */
/* { dg-options "-O2" } */

int foo (void);

int
main (void)
{
  return foo ();
}

/* { dg-final { scan-assembler "foo@PLT" } } */
