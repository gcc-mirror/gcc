// PR c++/54542
// { dg-do compile { target c++11 } }

template <class T>
void f(decltype(new T(1, 2)) *)
{
  T(1, 2);
}

template <class T>
void f(...)
{}

int main()
{
  f<int>(0);
}
