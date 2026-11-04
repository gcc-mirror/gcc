/* PR target/128409 */
/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fcf-protection=branch -march=x86-64" } */

void
foo (long offset, void *handler)
{
  __builtin_eh_return (offset, handler);
}

/* { dg-final { scan-assembler-not "\tjmp\[ \t\]+\\*%\[er\]cx" } } */
/* { dg-final { scan-assembler "\tret" } } */
