/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O -fschedule-insns2 -fselective-scheduling2" } */

struct S
{
  volatile long vl;
  int i;
};
struct S s1, s2;

void
foo (int j, int c)
{
  int i;
  for (i = 0; i <= j; i++)
    {
      if (c)
	s2.vl += s1.vl;
      s1 = s2;
    }
}
