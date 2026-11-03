/* PR target/128409 */
/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fcf-protection=return -march=x86-64" } */

void
foo (long offset, void *handler)
{
  __builtin_eh_return (offset, handler);
}

/* { dg-final { scan-assembler-not "\tret" } } */
/* { dg-final { scan-assembler "\tjmp\[ \t\]+\\*%\[er\]cx" } } */
