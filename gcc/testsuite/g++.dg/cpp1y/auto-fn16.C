// { dg-do compile { target c++1y } }

template<class,class> struct ST;
template<class T> struct ST<T,T> {};

int j;
auto x3 = []()->auto&& { return j; }; // OK: return type is int&

int main()
{
  ST<decltype(x3()),int&>();
}
