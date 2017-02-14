/* { dg-do compile { target { { powerpc*-*-* } && ilp32 } } } */
/* { dg-skip-if "" { powerpc*-*-aix* } { "*" } { "" } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-mcpu=power7 -O2" } */
/* { dg-final { scan-assembler-times "addze" 1 } } */

extern int fn2 ();
extern void fn3 ();
extern void fn4 (int);
int a, c, d, f, g, h, i, j, k, l, m, n;
struct
{
  int escape;
} *b;
int e[8];
void
fn1 (int p1, int p2)
{
  int o = a;
  for (; f; f++)
    {
      int p;
      if (e[h])
      continue;
      if (fn2 (o, d, l, n, p1, i, j, k, 0==0))
      continue;
      p = p2;
      if (b[g].escape)
      p++;
      fn3 ("", c, m);
      if (k)
      fn4 (p);
    }
}
