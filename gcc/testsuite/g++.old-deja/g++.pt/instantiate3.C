// Build don't link:

template <class T>
struct S
{
  S(const T&) {}
  S(int, long);
};

template S<double>::S(const double&);
