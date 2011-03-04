// PR c++/47971

template <typename> struct S
{
  typedef double T;
  S () { T ().~T (); }
};

S<double> s;
