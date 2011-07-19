// PR c++/49165
// { dg-do run }

extern "C" void abort ();

int
foo (bool x, int y)
{
  if (y < 10 && (x ? 1 : throw 1))
    y++;
  if (y > 20 || (x ? 1 : throw 2))
    y++;
  return y;
}

int
main ()
{
  if (foo (true, 0) != 2
      || foo (true, 10) != 11
      || foo (false, 30) != 31)
    abort ();
  try
    {
      foo (false, 0);
      abort ();
    }
  catch (int i)
    {
      if (i != 1)
	abort ();
    }
  try
    {
      foo (false, 10);
      abort ();
    }
  catch (int i)
    {
      if (i != 2)
	abort ();
    }
}
