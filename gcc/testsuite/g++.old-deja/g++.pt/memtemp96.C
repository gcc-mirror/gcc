// { dg-do run  }
// Test for partial specialization of a member function template.
// Origin: Jason Merrill <jason@cygnus.com>

template <class T> struct A {
  template <class U> int f(U) { return 42; }
};

template <>
template <class U>
int A<char>::f(U);

template <>
template <class U>
int A<double>::f(U) { return 24; }

int main ()
{
  A<int> ai;
  if (ai.f(0) != 42)
    return 1;

  A<double> ad;
  if (ad.f(0) != 24)
    return 1;

  A<char> ac;
  if (ac.f(0) != 36)
    return 1;
}

template <>
template <class U>
int A<char>::f(U) { return 36; }
