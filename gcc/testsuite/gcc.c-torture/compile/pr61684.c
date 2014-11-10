/* PR tree-optimization/61684 */
/* { dg-xfail-if "ptxas crashes" { nvptx-*-* } { "*" } { "-O0" "-O1" "-Os" } } */

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
