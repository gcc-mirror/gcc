// { dg-do compile { target c++2a } }

template<typename T>
concept type = true;

template<typename T, typename U>
concept same_as = __is_same_as(T, U);

template<typename T>
concept integral = __is_same_as(T, int);

template<typename... Ts>
concept all_integral = (integral<Ts> && ...);

void f1(integral auto... args) { }
void f2(all_integral auto... args) { }

template<type T> requires true
void f3(T, integral auto... args) { }

template<type T>
struct S
{
  void f1(integral auto... args) { }
  void f2(all_integral auto... args) { }

  template<type U> requires true
  void f3(U, integral auto... args) { }
};

int main()
{
  f1(1, 2, 3);
  f1(1, 2, 3u); // { dg-error "" }
  f2(1, 2, 3);
  f2(1, 2, 3u); // { dg-error "" }
  f3(1, 2, 3);
  f3(1, 2, 3u); // { dg-error "" }
  f3(1u, 2, 3);

  S<void> s;
  s.f1(1, 2, 3);
  s.f1(1, 2, 3u); // { dg-error "no matching function" }
  s.f2(1, 2, 3);
  s.f2(1, 2, 3u); // { dg-error "no matching function" }
  s.f3(1, 2, 3);
  s.f3(1, 2, 3u); // { dg-error "no matching function" }
  s.f3(1u, 2, 3);
}
