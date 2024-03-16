void abort (void);
void exit (int);

struct a { unsigned int bitfield : 1; };

unsigned int x;

int
main(void)
{
  struct a a = {0};
  x = 0xbeef;
  a.bitfield |= x;
  if (a.bitfield != 1)
    abort ();
  exit (0);
}
