// Build don't link:

struct S1
{
  template <class T>
  void f(T t1, T t2);
};


template <>
void S1::f(int i1, int i2);

template <class U>
struct S2
{
  template <class T>
  void f(T t1, T t2);
};

template <>
template <>
void S2<char>::f(int i1, int i2);

void h()
{
  S1 s1;
  s1.f(3, 'c'); // ERROR - no matching function

  S2<char> s2;
  s2.f(3, 'c'); // ERROR - no matching function
}
