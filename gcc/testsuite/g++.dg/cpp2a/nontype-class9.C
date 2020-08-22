// PR c++/88216
// { dg-do compile { target c++20 } }

template <class T, class U> struct same;
template <class T> struct same<T,T> {};

struct T { };

template <T t>
struct U { };

template <T t>
void f (U<t>)
{
  same<T,decltype(t)> s;
  same<const T&,decltype((t))> s2;
}

template<T t>
U<t> u;

T t;
U<t> u2;

void
g ()
{
  f<t>(u2);
}
