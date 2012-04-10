// { dg-options -std=c++1y }

template <class T, class U> struct ST;
template <class T> struct ST<T,T> {};

int g(int);
char& g(char);
double&& g(double);

template <class T> auto&& f(T t)
{ return g(t); }		// { dg-warning "reference to temporary" }

int main()
{
  ST<decltype(f(1)),int&&>();
  ST<decltype(f('\0')),char&>();
  ST<decltype(f(1.0)),double&&>();
}
