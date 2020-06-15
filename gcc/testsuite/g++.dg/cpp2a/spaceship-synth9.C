// Test that most properties of <=> are copied to ==.
// { dg-do compile { target c++20 } }

#include <compare>

template<typename T> struct X {
  T t;
  friend consteval std::partial_ordering operator<=>(X, X) requires (sizeof(T) != 1) = default;
  // implicitly declares: friend constexpr bool operator==(X, X) requires (sizeof(T) != 1) = default;
};

template<typename T> struct Y {
  [[nodiscard]] virtual std::strong_ordering operator<=>(const Y&) const = default;
  // implicitly declares: [[nodiscard]] virtual bool operator==(const Y&) const = default;
};

struct Z: Y<int>
{
  bool operator==(const Y&) const noexcept override;
};

int main()
{
  X<char>() == X<char>();	// { dg-error "no match" }
  X<int> x; x == x;		// { dg-error "x' is not usable in a constant expression" }
  Y<int>()  == Y<int>();	// { dg-warning "nodiscard" }
}
