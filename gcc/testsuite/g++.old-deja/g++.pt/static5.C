// Build don't link:

template <class T>
struct S
{
  static const T t = 3; // ERROR - initializing non-integral type
};

double d = S<double>::t;
