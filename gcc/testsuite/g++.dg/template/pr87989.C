// PR c++/87989
// { dg-do link }
// Resolved to template instantiation rather than non-template fn.

struct X {
  template <class T> operator T() const; // no definition
  operator float() const {return 0.f;}
};

template <class T>
T f(const X &x) {
  // Resoved in error to X::operator float<float>() const`
  // instead of correct `X::operator float() const
  return x.operator T();
}

int main ()
{
  return f<float>(X ());
}
