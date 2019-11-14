// { dg-do compile { target c++2a } }

#include <compare>

template <class T>
struct D
{
  T i;
  auto operator<=>(D) const = default; // { dg-error "defaulted member" }
  bool operator==(D) const = default; // { dg-error "defaulted member" }
  bool operator!=(D) const = default; // { dg-error "defaulted member" }
  bool operator<(D) const = default; // { dg-error "defaulted member" }
  bool operator<=(D) const = default; // { dg-error "defaulted member" }
  bool operator>(D) const = default; // { dg-error "defaulted member" }
  bool operator>=(D) const = default; // { dg-error "defaulted member" }
};

template <class T>
struct E
{
  T i;
  friend auto operator<=>(const E&, E) = default; // { dg-error "not both" }
  friend bool operator==(const E&, E) = default; // { dg-error "not both" }
  friend bool operator!=(const E&, E) = default; // { dg-error "not both" }
  friend bool operator<(E, const E&) = default; // { dg-error "not both" }
  friend bool operator<=(E, const E&) = default; // { dg-error "not both" }
  friend bool operator>(E, const E&) = default; // { dg-error "not both" }
  friend bool operator>=(E, const E&) = default; // { dg-error "not both" }
};
