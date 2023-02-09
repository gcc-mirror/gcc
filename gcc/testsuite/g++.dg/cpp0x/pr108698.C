// PR c++/108698
// { dg-do compile { target c++11 } }

template <typename T>
decltype (T () * T () + 1.0) foo ()
{
  return T () * T () + 1.0;
}

void
bar ()
{
  foo <float> ();
  foo <double> ();
  foo <long double> ();
}
