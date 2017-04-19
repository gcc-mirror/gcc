// { dg-do assemble  }
// { dg-options "" }

template <class ARRY>
inline unsigned int asize(ARRY &a) // { dg-message "note" }
{
  return sizeof(a) / sizeof(a[0]);
}

void f(unsigned int n) {
  int x[n];

  asize(x); // { dg-error "" } no matching function
  // { dg-message "(candidate|not a valid template argument)" "candidate note" { target *-*-* } .-1 }
}
