// { dg-do compile { target c++14 } }

template <class T, class U> struct ST;
template <class T> struct ST<T,T> {};

int g(int);
char& g(char);
double&& g(double);

template <class T> auto&& f(T t)
{ return g(t); }		// { dg-message "reference to temporary" }

int main()
{
  ST<decltype(f(1)),int&&>();	// { dg-message "required from here" }
  ST<decltype(f('\0')),char&>(); // { dg-bogus "required from here" }
  ST<decltype(f(1.0)),double&&>(); // { dg-bogus "required from here" }
}
