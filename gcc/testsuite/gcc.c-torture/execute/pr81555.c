/* PR tree-optimization/81555 */

unsigned int a = 1, d = 0xfaeU, e = 0xe376U;
_Bool b = 0, f = 1;
unsigned char g = 1;

void
foo (void)
{
  _Bool c = a != b;
  if (c)
    f = 0;
  if (e & c & (unsigned char)d & c)
    g = 0;
}

int
main ()
{
  foo ();
  if (f || g != 1)
    __builtin_abort ();
  return 0;
}
