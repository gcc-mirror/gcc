/* Verify that we optimize to conditional traps.  */
/* { dg-options "-O" } */
/* { dg-do compile { target rs6000-*-* powerpc-*-* sparc*-*-* } } */
/* { dg-final { scan-assembler-not "^\t(trap|ta)\[ \t\]" } } */

void f1(int p)
{
  if (p)
    __builtin_trap();
}

void f2(int p)
{
  if (p)
    __builtin_trap();
  else
    bar();
}

void f3(int p)
{
  if (p)
    bar();
  else
    __builtin_trap();
}

void f4(int p, int q)
{
  if (p)
    {
      bar();
      if (q)
	bar();
    }
  else
    __builtin_trap();
}
