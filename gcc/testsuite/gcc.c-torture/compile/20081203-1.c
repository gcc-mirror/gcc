/* PR rtl-optimization/38281 */
/* Reporter: John Regehr <regehr@cs.utah.edu> */
/* Testcase by Jakub Jelinek <jakub@redhat.com> */

inline unsigned short
foo (unsigned short x, unsigned short y)
{
  if (y == 0)
    return x;
  return x / y;
}

unsigned short a, b, c;

extern int baz (int, int);

void
bar (void)
{
  int d = 0x3D75D162;
  a = foo (b > d, baz (0, 1));
  for (c = 0; c; c = 1)
    ;
}
