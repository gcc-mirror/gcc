// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

// Check shorthand notation.

template<typename T>
  concept Type = true;

template<typename T, typename U>
  concept Same = __is_same_as(T, U);

template<Same<int> T> struct S1 { };
template<typename T, Same<T> U> struct S2 { };

void f(Same<int> auto q) { }
void g(Type auto a, Same<decltype(a)> auto b) { }

int main() {
  S1<char> s1;      // { dg-error "constraint|invalid" }
  S2<int, char> s2; // { dg-error "constraint|invalid" }

  f('a');    // { dg-error "no match" }
  g(0, 'a'); // { dg-error "no match" }
}
