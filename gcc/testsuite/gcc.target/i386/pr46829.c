/* { dg-do compile } */
/* { dg-options "-O2 -fschedule-insns" } */

struct S
{
  int i, j;
};

extern struct S s[];

extern void bar (int, ...);

void
foo (int n)
{
  while (s[n].i)
    bar (0, n, s[n].j, s, s[n].i / s[n].j);
}
