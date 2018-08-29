/* PR tree-optimization/55921 */
/* { dg-skip-if "Not enough registers" { "pdp11-*-*" } } */

typedef union
{
  _Complex float cf;
  long long ll;
} ucf;

void
foo (ucf *in, ucf *out, _Complex float r)
{
  int i;
  ucf ucf1;
  _Complex float cf;

  ucf1.ll = in[i].ll;
  __asm ("" : "=r" (cf) : "r" (ucf1.ll));
  cf *= r;
  __asm ("" : "=r" (ucf1.ll) : "r" (cf));
  out[i].ll = ucf1.ll;
}
