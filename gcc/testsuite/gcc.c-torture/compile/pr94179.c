/* PR c/94179 */

struct S { char c, d, e[64]; } a;

unsigned char *
foo (int b)
{
  return (unsigned char *)((char *)&a.e[b != 0] - (char *)&((struct S *)0)->d);
}
