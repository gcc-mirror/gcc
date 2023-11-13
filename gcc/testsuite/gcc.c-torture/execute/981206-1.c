/* Verify unaligned address aliasing on Alpha EV[45].  */

void abort (void);
void exit (int);

static unsigned short x, y;

void foo()
{
  x = 0x345;
  y = 0x567;
}

int main()
{
  foo ();
  if (x != 0x345 || y != 0x567)
    abort ();
  exit (0);
}
