/* PR target/45913 */
/* { dg-do compile } */
/* { dg-options "-O2 -fselective-scheduling2 -fsel-sched-pipelining -fsel-sched-pipelining-outer-loops" } */

extern void bar (int, int);

int ss[128];

void
foo (int i, int j, int k, int *p1, int *p2)
{
  int s[128];
  __builtin_memcpy (s, ss, sizeof s);

  while (i--)
    {
      int a = s[i];
      while (j--)
	bar (k, p2[a]);
      j = s[i] & 0xFF;
      bar (p1[a], k);
    }
}
