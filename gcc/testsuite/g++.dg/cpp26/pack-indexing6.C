// P2662R3 - Pack Indexing
// PR c++/113798
// { dg-do compile { target c++26 } }

template<class, class> struct same_type;
template<class T> struct same_type<T, T> {};

void
fn1 (auto... Ts)
{
  decltype(auto) a1 = Ts...[0];
  same_type<decltype(a1), int>();
  decltype(auto) a2 = (Ts...[0]);
  same_type<decltype(a2), int&>();
}

template<auto... Is>
void
fn2 ()
{
  decltype(auto) a1 = Is...[0];
  same_type<decltype(a1), int>();
  decltype(auto) a2 = (Is...[0]);
  same_type<decltype(a2), int>();
  decltype(auto) a3 = Is...[1];
  same_type<decltype(a3), unsigned int>();
  decltype(auto) a4 = (Is...[1]);
  same_type<decltype(a4), unsigned int>();
  decltype(auto) a5 = Is...[2];
  same_type<decltype(a5), double>();
  decltype(auto) a6 = (Is...[2]);
  same_type<decltype(a6), double>();
  decltype(auto) a7 = Is...[3];
  same_type<decltype(a7), float>();
  decltype(auto) a8 = (Is...[3]);
  same_type<decltype(a8), float>();
  decltype(auto) a9 = Is...[4];
  same_type<decltype(a9), unsigned char>();
  decltype(auto) a10 = (Is...[4]);
  same_type<decltype(a10), unsigned char>();
}

static constexpr unsigned char c = 'A';

void
g ()
{
  int i = 42;
  fn1 (i, 42u);
  fn2<0, 1u, 2.0, 3.f, c>();
}
