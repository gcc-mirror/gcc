// PR c++/97121
// { dg-do compile { target c++20 } }

#include <compare>

class MyClass
{
  int mValue;	// { dg-error "three-way comparison of 'MyClass::mValue' has type 'std::strong_ordering', which does not convert to 'bool'" }

public:
  MyClass(int value): mValue(value) {}

  bool operator<=>(const MyClass&) const = default;	// { dg-error "invalid 'static_cast' from type 'const std::strong_ordering' to type 'bool'" }
};

int main()
{
  MyClass a = 10, b = 15;
  return (a < b);	// { dg-error "use of deleted function 'constexpr bool MyClass::operator<=>\\\(const MyClass&\\\) const'" }
}
