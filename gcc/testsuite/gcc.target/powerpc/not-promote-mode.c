/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O2" } */

extern void bar ();

void foo ()
{
  int i;
  for (i = 0; i < 10000; i++)
    bar ();
}

/* { dg-final { scan-assembler-not   {\mrldicl\M} } } */
