/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O1 -freorder-blocks -fschedule-insns2 -funswitch-loops -fselective-scheduling2 -fsel-sched-pipelining -funroll-all-loops" } */
void
foo1 (int *s)
{
  s[0] = s[1];
  while (s[6] - s[8])
    {
      s[6] -= s[8];
      if (s[8] || s[0])
	{
	  s[3] += s[0];
	  s[4] += s[1];
	}
      s[7]++;
    }
}
