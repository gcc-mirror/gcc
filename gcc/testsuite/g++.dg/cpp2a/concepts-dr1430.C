// PR c++/66092
// { dg-do compile { target c++2a } }

template <typename T, typename U, typename... Args>
  concept Similar = true;

template <typename... Args>
  requires Similar<Args...> // { dg-error "pack expansion" }
void foo( Args... args ) {}

int main()
{
  foo(1, 2, 3); // { dg-error "" }
}
