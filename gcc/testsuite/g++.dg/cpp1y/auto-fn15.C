// { dg-options "-std=c++1y -Wno-return-local-addr" }

template<class,class> struct same_type;
template<class T> struct same_type<T,T> {};

int& f();
int i;

decltype(auto) g() { return f(); }
decltype(auto) h1() { return i; }
decltype(auto) h2() { return (i); }
decltype(auto) h2a() { return 0,i; }

struct A { int i; };
A a;

decltype(auto) h3() { return a.i; }
decltype(auto) h4() { return (a.i); }

template <class T>
decltype(auto) h5(T t) { return t.i; }
template <class T>
decltype(auto) h6(T t) { return (t.i); }

int main()
{
  decltype(auto) i = f();
  same_type<decltype(i),int&>();
  decltype(auto) i2 = i;
  same_type<decltype(i2),int&>();
  decltype(auto) i3 = ::i;
  same_type<decltype(i3),int>();
  decltype(auto) i4 = (::i);
  same_type<decltype(i4),int&>();
  decltype(auto) i5 = a.i;
  same_type<decltype(i5),int>();
  decltype(auto) i6 = (a.i);
  same_type<decltype(i6),int&>();
  decltype(auto) i7 = true ? ::i : ::i;
  same_type<decltype(i7),int&>();

  same_type<decltype(g()),int&>();
  same_type<decltype(h1()),int>();
  same_type<decltype(h2()),int&>();
  same_type<decltype(h2a()),int&>();
  same_type<decltype(h3()),int>();
  same_type<decltype(h4()),int&>();
  same_type<decltype(h5(a)),int>();
  same_type<decltype(h6(a)),int&>();
}
