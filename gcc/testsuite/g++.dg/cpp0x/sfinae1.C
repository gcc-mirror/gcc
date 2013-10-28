// { dg-options "-std=c++11" }

template< typename T_VECTOR >
void f(const T_VECTOR &a, decltype(a[0]) t = 0);
template< typename T >
void f(const T &a, decltype(a*1) t = 0);

int main() {
  int c;
  f(c);
}
