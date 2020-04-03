// { dg-do compile { target c++2a } }

template<typename T>
  inline constexpr bool foo_v = false;

template<typename T>
  concept foo = (bool)(foo_v<T> | foo_v<T&>);

template<typename... Ts>
requires (foo<Ts> && ...)
void
bar() // { dg-message "with Ts = .int, char... evaluated to .false." }
{ }

template<int>
struct S { };

template<int... Is>
requires (foo<S<Is>> && ...)
void
baz() // { dg-message "with Is = .2, 3, 4... evaluated to .false." }
{ }

void
baz()
{
  bar<int, char>(); // { dg-error "unsatisfied constraints" }
  baz<2,3,4>(); // { dg-error "unsatisfied constraints" }
}
