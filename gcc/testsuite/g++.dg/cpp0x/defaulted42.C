// DR 941
// { dg-require-effective-target c++11 }

template <class T> T f(T) = delete;
template<> int f(int) { return 42; }

int main()
{
  f(42);
}
