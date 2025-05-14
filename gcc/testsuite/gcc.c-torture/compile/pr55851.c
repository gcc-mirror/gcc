/* PR middle-end/55851 */

enum { A = 1UL, B = -1UL } var = A;
void foo (char *);

void
test (void)
{
  char vla[1][var];
  vla[0][0] = 1;
  foo (&vla[0][0]);
}
