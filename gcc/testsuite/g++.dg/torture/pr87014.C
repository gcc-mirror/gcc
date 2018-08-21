// { dg-do run }

void
fillstack ()
{
  long long foo[] =
    {
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    };
}

void
f (long long=-1,long long=-1,long long=-1,long long=-1,
   long long=-1,long long=-1,long long arg7_on_stack=-1)
{
  throw 0;
}

void
g()
{
  try
    {
      f ();
    }
  catch (int)
    {
    }
}

int
main()
{
  fillstack ();
  g ();
  return 0;
}
