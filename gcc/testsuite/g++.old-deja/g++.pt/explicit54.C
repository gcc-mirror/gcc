extern "C" void abort ();

template <int a> inline int fact2 ();

template <int a> inline int fact ()
{
  return a * fact2<a-1> ();
}

template <> inline int fact<1> ()
{
  return 1;
}

template <int a> inline int fact2 ()
{
  return a*fact<a-1>();
}

template <> inline int fact2<1> ()
{
  return 1;
}

int main()
{
  if (fact<3> () != 6 || fact<1> () != 1
      || fact<3> () != 6 || fact<1> () != 1 || fact<1+0> () != 1)
    abort ();
  if (fact2<3> () != 6 || fact2<1> () != 1
      || fact2<3> () != 6 || fact2<1> () != 1 || fact2<1+0> () != 1)
    abort ();
  if (fact2<4> () != 24 || fact<4> () != 24)
    abort ();
}
