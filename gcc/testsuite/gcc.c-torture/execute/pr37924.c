/* PR c/37924 */

extern void abort (void);

signed char a;
unsigned char b;

int
test1 (void)
{
  int c = -1;
  return ((unsigned int) (a ^ c)) >> 9;
}

int
test2 (void)
{
  int c = -1;
  return ((unsigned int) (b ^ c)) >> 9;
}

int
main (void)
{
  a = 0;
  if (test1 () != (-1U >> 9))
    abort ();
  a = 0x40;
  if (test1 () != (-1U >> 9))
    abort ();
  a = 0x80;
  if (test1 () != (a < 0) ? 0 : (-1U >> 9))
    abort ();
  a = 0xff;
  if (test1 () != (a < 0) ? 0 : (-1U >> 9))
    abort ();
  b = 0;
  if (test2 () != (-1U >> 9))
    abort ();
  b = 0x40;
  if (test2 () != (-1U >> 9))
    abort ();
  b = 0x80;
  if (test2 () != (-1U >> 9))
    abort ();
  b = 0xff;
  if (test2 () != (-1U >> 9))
    abort ();
  return 0;
}
