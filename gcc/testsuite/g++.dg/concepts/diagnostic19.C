// Verify pretty printing of class NTTP objects.
// PR c++/111471
// { dg-do compile { target c++20 } }

struct A { bool value; };

template<A V>
  requires (V.value) // { dg-message {'\(V\).value \[with V = A\{false\}\]'} }
void f();

template<A V> struct B { static constexpr auto value = V.value; };

template<class T>
  requires T::value // { dg-message {'T::value \[with T = B<A\{false\}>\]'} }
void g();

int main() {
  f<A{false}>(); // { dg-error "no match" }
  g<B<A{false}>>(); // { dg-error "no match" } 
}
