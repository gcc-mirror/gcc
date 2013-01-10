/* PR tree-optimization/55921 */

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
  __asm ("" : "=r" (cf) : "0" (ucf1.ll));
  cf *= r;
  __asm ("" : "=r" (ucf1.ll) : "0" (cf));
  out[i].ll = ucf1.ll;
}
