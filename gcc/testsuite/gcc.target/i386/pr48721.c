/* PR rtl-optimization/48721 */
/* { dg-do compile } */
/* { dg-options "-O -foptimize-sibling-calls -fsched2-use-superblocks -fschedule-insns2 -mtune=core2" } */

extern unsigned char a[];
extern int b[], d[], e[], f[], g[], *h[], m[], *n[], o[];
extern char c[];

struct S
{
  unsigned char s1;
  int s2, s3, s4, s5, s6, s7, s8;
};

__attribute__((noinline, noclone)) int
foo (int x)
{
  return 0;
}

int
bar (int x, struct S *y)
{
  int z;
  switch (x)
    {
    case 1:
    case 2:
      {
	int t2, t4, t5, t6, t7, t8;
	z = o[y->s8 * 6];
	t8 = *n[m[x] * 5];
	t4 = *h[y->s7];
	t7 = z;
	z = g[f[x] + y->s6];
	t6 = e[y->s5];
	t5 = d[c[x] + y->s3 * 17];
	if (z)
	  t2 = b[z];
	if (a[z] != y->s1)
	  return foo (x);
	y->s8 = t8;
	y->s4 = t4;
	y->s7 = t7;
	y->s6 = t6;
	y->s5 = t5;
	y->s2 = t2;
      }
    }
  return 0;
}
