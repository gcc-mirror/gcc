// PR c++/86946, DR 1321
// { dg-do compile { target c++11 } }

int d(int, int);
template <long> class e {};
template <unsigned long f, unsigned b, typename> e<sizeof(d(f, b))> d();
template <unsigned long f, unsigned b, typename> e<d(f, b)> d();

template <class T, class U> constexpr T d2(T, U) { return 42; }
template <unsigned long f, unsigned b, typename> e<d2(f, b)> d2();
template <unsigned long f, unsigned b, typename> e<d2(f, b)> d2();

template <typename a, typename c> a d3(a, c);
template <unsigned long f, unsigned b, typename> e<sizeof(d3(f, b))> d3();
template <unsigned long f, unsigned b, typename> e<sizeof(d3(f, b))> d3();


int main()
{
  d<1,2,int>();
  d2<1,2,int>();
  d3<1,2,int>();
}
