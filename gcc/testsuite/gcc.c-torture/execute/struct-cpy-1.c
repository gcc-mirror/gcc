/* powerpc64-linux gcc miscompiled this due to rs6000.c:expand_block_move
   not setting mem aliasing info correctly for the code implementing the
   structure assignment.  */

struct termios
{
  unsigned int a;
  unsigned int b;
  unsigned int c;
  unsigned int d;
  unsigned char pad[28];
};

struct tty_driver
{
  unsigned char pad1[38];
  struct termios t __attribute__ ((aligned (8)));
};

static struct termios zero_t;
static struct tty_driver pty;

void ini (void)
{
  pty.t = zero_t;
  pty.t.a = 1;
  pty.t.b = 2;
  pty.t.c = 3;
  pty.t.d = 4;
}

int main (void)
{
  extern void abort (void);

  ini ();
  if (pty.t.a != 1
      || pty.t.b != 2
      || pty.t.c != 3
      || pty.t.d != 4)
    abort ();
  return 0;
}
