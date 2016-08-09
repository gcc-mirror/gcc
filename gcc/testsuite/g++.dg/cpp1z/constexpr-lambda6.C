// Testcase from P0170R1
// { dg-do run }
// { dg-options -std=c++1z }

auto monoid = [](auto v) { return [=] { return v; }; };
auto add = [](auto m1) constexpr {
  auto ret = m1();
  return [=](auto m2) mutable {
    auto m1val = m1();
    auto plus = [=] (auto m2val) mutable constexpr
      { return m1val += m2val; };
    ret = plus(m2());
    return monoid(ret);
  };
};

int main()
{
  constexpr auto zero = monoid(0);
  constexpr auto one = monoid(1);
  static_assert(add(one)(zero)() == one()); // OK
  // Since 'two' below is not declared constexpr, an evaluation of its constexpr
  // member function call operator can not perform an lvalue-to-rvalue conversion
  // on one of its subobjects (that represents its capture) in a constant
  // expression.
  auto two = monoid(2);
  if (!(two() == 2)) __builtin_abort(); // OK, not a constant expression.
  static_assert(add(one)(one)() == two()); // { dg-error "" } two() is not a constant expression
  static_assert(add(one)(one)() == monoid(2)()); // OK
}
