// P2662R3 - Pack Indexing
// PR c++/113798
// { dg-do compile { target c++26 } }

template<class, class> struct same_type;
template<class T> struct same_type<T, T> {};

void
fn1 (auto... Ts)
{
  same_type<decltype(Ts...[0]), int>();
  same_type<decltype((Ts...[0])), int&>();
  same_type<decltype(Ts...[1]), unsigned int>();
  same_type<decltype((Ts...[1])), unsigned int&>();
}

template<auto... Is>
void
fn2 ()
{
  same_type<decltype(Is...[0]), int>();
  same_type<decltype((Is...[0])), int>();
  same_type<decltype(Is...[1]), unsigned int>();
  same_type<decltype((Is...[1])), unsigned int>();
  same_type<decltype(Is...[2]), double>();
  same_type<decltype((Is...[2])), double>();
  same_type<decltype(Is...[3]), float>();
  same_type<decltype((Is...[3])), float>();
  same_type<decltype(Is...[4]), unsigned char>();
  same_type<decltype((Is...[4])), unsigned char>();
}

static constexpr unsigned char c = 'A';

void
g ()
{
  int i = 42;
  fn1 (i, 42u);
  fn2<0, 1u, 2.0, 3.f, c>();
}
