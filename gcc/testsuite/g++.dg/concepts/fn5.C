// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

// Check shorthand notation.

template<typename T>
  concept bool Type() { return true; }

template<typename T, typename U>
  concept bool Same() { return __is_same_as(T, U); }

template<Same<int> T> struct S1 { };
template<typename T, Same<T> U> struct S2 { };

void f(Same<int> q) { }
void g(Type a, Same<decltype(a)> b) { }

int main() {
  S1<char> s1;      // { dg-error "constraint|invalid" }
  S2<int, char> s2; // { dg-error "constraint|invalid" }

  f('a');    // { dg-error "no match" }
  g(0, 'a'); // { dg-error "no match" }
}
