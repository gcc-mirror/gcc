/* Make sure that the H8 backend does not generate a non-atomic stack
   adjustment.  */
/* { dg-do compile { target h8300-*-* } } */
/* { dg-options "" } */

void foo (short, short, short, short, short, short);

void
bar ()
{
  foo (0, 0, 0, 0, 0, 0);
}

/* { dg-final { scan-assembler-not "addx" } } */
