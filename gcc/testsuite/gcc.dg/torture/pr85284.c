/* { dg-do run } */

static int p[48], v;

int
main ()
{
  p[32] = 1;
  for (int i = 48; i--;)
    {
      if (!p[i])
	continue;
      if ((i & 7) > 2)
	break;
      v = i & 1;
    }
  if (v != 0)
    __builtin_abort ();
  return 0;
}
