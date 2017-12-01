// PR c++/79228
// { dg-do compile { target c++14 } }
// { dg-options "" }

template <class,class> struct same;
template <class T> struct same<T,T> { };

int main()
{
  same<decltype(0i),__complex int>{};
  same<decltype(0.0i),__complex double>{};
  same<decltype(0.0if),__complex float>{};
  same<decltype(0.0il),__complex long double>{};
}
