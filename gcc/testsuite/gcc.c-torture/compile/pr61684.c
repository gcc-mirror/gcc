/* PR tree-optimization/61684 */

int a, c;
static int *b = 0;
short d;
static short **e = 0;

void
foo ()
{
  for (; c < 1; c++)
    ;
  *e = &d;
  a = d && (c && 1) & *b;
}
