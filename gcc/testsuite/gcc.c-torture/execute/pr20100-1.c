/* PR tree-optimization/20100
   Pure function being treated as const.
   Author: Hans-Peter Nilsson.  */

static unsigned short g = 0;
static unsigned short p = 0;
unsigned char e;

static unsigned short
next_g (void)
{
  return g == e - 1 ? 0 : g + 1;
}

static unsigned short
curr_p (void)
{
  return p;
}

static unsigned short
inc_g (void)
{
  return g = next_g ();
}

static unsigned short
curr_g (void)
{
  return g;
}

static char
ring_empty (void)
{
  if (curr_p () == curr_g ())
    return 1;
  else
    return 0;
}

char
frob (unsigned short a, unsigned short b)
{
  g = a;
  p = b;
  inc_g ();
  return ring_empty ();
}

unsigned short
get_n (void)
{
  unsigned short n = 0;
  unsigned short org_g;
  org_g = curr_g ();
  while (!ring_empty () && n < 5)
    {
      inc_g ();
      n++;
    }

  return n;
}

void abort (void);
void exit (int);
int main (void)
{
  e = 3;
  if (frob (0, 2) != 0 || g != 1 || p != 2 || e != 3
      || get_n () != 1
      || g != 2 || p != 2)
    abort ();
  exit (0);
}
