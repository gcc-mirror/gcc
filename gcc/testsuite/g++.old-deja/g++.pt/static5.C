// { dg-do assemble  }

template <class T>
struct S
{
  static const T t = 3; // { dg-error "" } initializing non-integral type
};

double d = S<double>::t;
