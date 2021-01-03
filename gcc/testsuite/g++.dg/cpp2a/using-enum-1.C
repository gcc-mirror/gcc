// Test of using an enumerator.
// { dg-do compile { target c++2a } }

// using ENUM::V;
enum class E {v};

using E::v;
using E::v; // OK

E a = v;

class C
{
  using E::v;			// { dg-message "declared private here" }

  static inline const E m = v;
};

E b = C::v;			// { dg-error "private" }

struct B
{
  enum E {e};
  enum class EC {f};
  using EC::f;
};

struct D
{
private:
  using B::e;			// { dg-message "declared private here" }
  using B::f;			// { dg-message "declared private here" }
};

struct F : D
{
  static inline const auto bad1 = e; // { dg-error "private" }
  static inline const auto bad2 = f; // { dg-error "private" }

  static inline const auto ok1 = B::e;
  static inline const auto ok2 = B::f;
  static inline const auto also_ok1 = B::E::e;
  static inline const auto also_ok2 = B::EC::f;
};

using B::e;
auto bob = e;

struct Q
{
  using B::e;
};
using Q::e;			// OK

using D::e;			// { dg-error "private" }

template <class T>
struct X : T
{
  using T::e;
};
auto fob = X<Q>::e;
