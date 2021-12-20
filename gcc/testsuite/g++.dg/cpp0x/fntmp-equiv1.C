// PR c++/86946, DR 1321
// { dg-do compile { target c++11 } }

int d(int, int);
template <long> class e {};
template <class T> e<sizeof(d(T{}, T{}))> d(...);
template <class T> e<d(T{}, T{})> d(...);

template <class T, class U> constexpr T d2(T, U) { return 42; }
template <class T> e<d2(T{}, T{})> d2(...);
template <class T> e<d2(T{}, T{})> d2(...);

template <typename a, typename c> a d3(a, c);
template <class T> e<sizeof(d3(T{}, T{}))> d3(...);
template <class T> e<sizeof(d3(T{}, T{}))> d3(...);


int main()
{
  d<int>();
  d2<int>();
  d3<int>();
}
