// { dg-do compile { target c++2a } }

template<typename T>
  inline constexpr bool foo_v = false;

template<typename T>
  concept foo = (bool)(foo_v<T> | foo_v<T&>);

template<typename... Ts>
requires (foo<Ts> && ...) // { dg-message "with Ts = .int, char... evaluated to .false." }
void
bar()
{ }

template<int>
struct S { };

template<int... Is>
requires (foo<S<Is>> && ...) // { dg-message "with Is = .2, 3, 4... evaluated to .false." }
void
baz()
{ }

void
baz()
{
  bar<int, char>(); // { dg-error "no match" }
  baz<2,3,4>(); // { dg-error "no match" }
}
