extern "C" void abort ();

template <int a> inline int fact ()
{
  return a * fact<a-1> ();
}

template <> inline int fact<1> ()
{
  return 1;
}

int main()
{
  if (fact<3> () != 6 || fact<1> () != 1
      || fact<3> () != 6 || fact<1> () != 1 || fact<1+0> () != 1)
    abort ();
}
