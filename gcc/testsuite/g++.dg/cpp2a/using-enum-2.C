// Test of 'using enum' in different scopes.
// { dg-do compile { target c++20 } }

namespace N
{
  enum class E { e, f };
}

int main()
{
  using enum N::E;
  static_assert (e < f);
}

struct A
{
  using enum N::E;
  static_assert (e < f);
};

namespace M
{
  using enum N::E;
  static_assert (e < f);

  enum class X: int;		// { dg-message "opaque" }
  using enum X;			// { dg-error "enum-specifier" }
}

template <class T>
void f()
{
  using enum N::E;
  static_assert (e < f);
}

template <class T>
struct AT
{
  using enum N::E;
  static_assert (e < f);
};

template <class T>
struct BT
{
  using enum T::E;		// { dg-error "dependent" }
};
