/* PR target/60072 */

int c = 1;

__attribute__ ((optimize (1)))
static int *foo (int *p)
{
  return p;
}

int
main ()
{
  *foo (&c) = 2;
  return c - 2;
}
