/* PR middle-end/52979 */

struct S
{
  unsigned int a : 16, b : 16, c : 16, d : 16, e : 14;
  unsigned int f : 4, g : 14, h : 8;
  char i;
  int j;
};

void
foo (struct S *s)
{
  s->f = 1;
}
