/* PR tree-optimization/111469 */

long f;
char *g;
__attribute__((noinline))
char o() {
  char l;
  while (f)
    ;
  l = *g;
  return l;
}

/* factor_out_conditional_conversion is able to remove the casts
   from the 2 bbs (correctly)
   but then minmax_replacement should not optimize this to a MIN_EXPR
   as o has side effects. */

__attribute__((noinline))
unsigned short gg(unsigned short a, unsigned short b)
{
  short d;
  if (a > b)
  {
    d= b;
  }
  else
  {
    o();
    d = a;
  }
  return d;
}

int main(void)
{
  gg(3, 2);
}
