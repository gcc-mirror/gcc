// { dg-do assemble  }

struct S1
{
  template <class T>
  void f(T t1, T t2);		// { dg-message "note" }
};


template <>
void S1::f(int i1, int i2);

template <class U>
struct S2
{
  template <class T>
  void f(T t1, T t2);		// { dg-message "note" }
};

template <>
template <>
void S2<char>::f(int i1, int i2);

void h()
{
  S1 s1;
  s1.f(3, 'c'); // { dg-error "" } no matching function
  // { dg-message "(candidate|deduced conflicting types)" "candidate note" { target *-*-* } .-1 }

  S2<char> s2;
  s2.f(3, 'c'); // { dg-error "" } no matching function
  // { dg-message "(candidate|deduced conflicting types)" "candidate note" { target *-*-* } .-1 }
}
