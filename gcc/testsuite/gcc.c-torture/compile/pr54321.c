/* PR tree-optimization/54321 */
struct S { char s[0]; } *a;

void
foo (void)
{
  char *b = a->s;
  int c = 0;
  b[0] = 0;
  while (++c < 9)
    b[c] = 255;
}
