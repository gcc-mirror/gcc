// { dg-do assemble  }
// { dg-options "" }

template <class ARRY>
inline unsigned int asize(ARRY &a)
{
  return sizeof(a) / sizeof(a[0]);
}

void f(unsigned int n) {
  int x[n];

  asize(x); // { dg-error "" } no matching function
}
