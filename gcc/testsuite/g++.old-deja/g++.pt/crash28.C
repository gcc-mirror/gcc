// Build don't link:
// Special g++ Options:

template <class ARRY>
inline unsigned int asize(ARRY &a)
{
  return sizeof(a) / sizeof(a[0]);
}

int f(unsigned int n) {
  int x[n];

  asize(x); // ERROR - no matching function
};
