// { dg-do run  }
template <class T>
void f(T t);

template <class T>
void f(T* t);

template <>
void f(int* ip) {}

struct S1
{
  template <class T>
  void f(T t);

  template <class T>
  void f(T* t);
};

template <>
void S1::f(int* ip) {}

template <class U>
struct S2
{
  template <class T>
  void f(T t);

  template <class T>
  void f(T* t);
};

template <>
template <>
void S2<double>::f(int* ip) {}

int main()
{
  int* ip;
  S1 s1;
  s1.f(ip);
  S2<double> s2;
  s2.f(ip);
}
