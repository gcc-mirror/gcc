/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv_zvl256b -mabi=ilp32d -O3" } */

int a, c, d, f, j;
int b[7];
long e;
char *g;
int *h;
long long *i;

void k() {
  int l[][1] = {{}, {1}, {1}};
  int *m = &d, *n = &l[0][0];

  for (; e;)
    {
      f = 3;

      for (; f >= 0; f--)
	{
	  *m &= b[f] >= 0;
	  j = a >= 2 ? 0 : 1 >> a;
	  *i |= j;
        }

	for (; c;)
	  *g = 0;
     }

  h = n;
}
