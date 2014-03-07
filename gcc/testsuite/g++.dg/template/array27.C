// PR c++/47808
// { dg-options "" }

template <typename T>
inline T abs (T const & x) { return x; }

template <typename T>
void f (T)
{
  typedef int ai[(abs(0.1) > 0) ? 1 : -1];
}

int main()
{
  f(1);
}
