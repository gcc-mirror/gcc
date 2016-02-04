/* PR middle-end/45852 */
/* { dg-options "-O2 -mcmodel=small" } */
/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-require-visibility "" } */

struct S { int s; };

volatile struct S globvar __attribute__((visibility ("hidden"))) = { -6 };

void
foo (void)
{
  globvar = globvar;
}

/* { dg-final { scan-assembler-times "globvar.%?rip" 2 } } */
