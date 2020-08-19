// PR c++/95568
// { dg-do compile { target c++20 } }
// CTAD with aggregates containing bit-fields.

template<class, class> struct same_type;
template<class T> struct same_type<T, T> {};

enum E { e };
enum class F { f };

template<typename T>
struct X {
  T a : 5;
};

template<typename>
void g()
{
  auto x = X{ 0 };
  same_type<decltype(x.a), int>();
  auto x2 = X{ E::e };
  same_type<decltype(x2.a), E>();
  auto x3 = X{ false };
  same_type<decltype(x3.a), bool>();
  auto x4 = X{ 0u };
  same_type<decltype(x4.a), unsigned int>();
  auto x5 = X{ F::f };
  same_type<decltype(x5.a), F>();
}

void
fn ()
{
  g<int>();
}
