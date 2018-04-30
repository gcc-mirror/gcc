/* { dg-do run } */
/* { dg-options "-O3 -fno-early-inlining"  } */

int array[128];

volatile int v = 0;
volatile int blah = 0;

int __attribute__((noipa))
obscured_zero ()
{
  return 0;
}

int __attribute__((noipa))
obscured_one ()
{
  return 1;
}

int __attribute__((noipa))
obscured_two ()
{
  return 2;
}

static
void cb1 (int l)
{
  v = 25;
}

static
void cb2 (int l)
{
  v = 125;
}

typedef void (*silly_callback)(int);

silly_callback __attribute__((noipa))
get_callback ()
{
  return cb1;
}

static void
f (int c, int l, silly_callback p)
{
  int i;

  for (i = 0; i < c; i++)
    array[i] = 455;

  for (i = 0; i < 200; i++)
    {
      p (l);
      if (obscured_one ())
	break;
    }

  if (l > 0)
    f (c * 2, l - 1, p);
  blah = l;
}

int
main (int argc, char *argv[])
{
  int i;
  for (i = 0; i < 1000; i++)
    {
      f (0, 5, get_callback ());
      if (v != 25)
	__builtin_abort ();
      if (obscured_one ())
	break;
    }

  for (i = 0; i < 1000; i++)
    {
      f (obscured_zero (), obscured_two (), cb2);
      if (v != 125)
	__builtin_abort ();
      if (obscured_one ())
	break;
    }

  return 0;
}
