/* { dg-do run } */
/* { dg-options "-O3 -fno-early-inlining"  } */

int array[128];

volatile int v = 0;
volatile int blah = 0;
volatile int counter = 0;

int __attribute__((noipa))
obscured_one ()
{
  return 1;
}

static void
f (int c, int l)
{
  int i;
  for (i = 0; i < c; i++)
    array[i] = 455;

  counter++;
  if (counter > 6)
    __builtin_abort ();

  v = l;
  if (l > 0)
    f (c, l - 1);
  blah = l;
}

int
main (int argc, char *argv[])
{
  int i;
  for (i = 0; i < 100; i++)
    {
      counter = 0;
      f (0, 5);
      if (obscured_one ())
	break;
    }

  return 0;
}
