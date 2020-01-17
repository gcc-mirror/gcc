/* PR target/93174 */

unsigned long long a[2];
void bar (void);

void
foo (int c)
{
  int e = c >> 2;
  a[0] += c;
  a[1] = a[0] < c;
  while (e--)
    bar ();
}
