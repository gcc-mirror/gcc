// { dg-do run  }
extern "C" void abort ();

template <int a> int fact ()
{
  return 0;
}

template <> int fact<1> ()
{
  return 1;
}

int main()
{
  if (fact<3> () != 0 || fact<1> () != 1
      || fact<3> () != 0 || fact<1> () != 1 || fact<1+0> () != 1)
    abort ();
}
