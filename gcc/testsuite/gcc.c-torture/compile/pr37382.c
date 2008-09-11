/* PR target/37382 */

void baz (char *);
int c;

void
bar (void)
{
  char a[2];
  int *ip = &c;
  char *p = a, *q = (char *) &ip;
  const char *r = q + 2;
  for (; q != r; p++, q++)
    *p = *q;
  baz (a);
}
