/* PR target/48774 */
/* { dg-do run } */
/* { dg-skip-if "PR 52125" { mips_rel } { "*" } { "" } } */
/* { dg-options "-O2 -funroll-loops" } */

extern void abort (void);
unsigned long int s[24]
  = { 12, ~1, 12, ~2, 12, ~4, 12, ~8, 12, ~16, 12, ~32,
      12, ~64, 12, ~128, 12, ~256, 12, ~512, 12, ~1024, 12, ~2048 };
struct { int n; unsigned long *e[12]; } g
  = { 12, { &s[0], &s[2], &s[4], &s[6], &s[8], &s[10], &s[12], &s[14],
	    &s[16], &s[18], &s[20], &s[22] } };
int c[12];

__attribute__((noinline, noclone)) void
foo (void)
{
  int i, j;
  for (i = 0; i < g.n; i++)
    for (j = 0; j < g.n; j++)
      {
	if (i == j && j < g.e[0][0] && (g.e[i][1] & (1UL << j)))
	  abort ();
	if (j < g.e[0][0] && (g.e[i][1] & (1UL << j)))
	  c[i]++;
      }
}

int
main ()
{
  int i;
  asm volatile ("" : "+m" (s), "+m" (g), "+m" (c));
  foo ();
  for (i = 0; i < 12; i++)
    if (c[i] != 11)
      abort ();
  return 0;
}
