// PR c++/111485
// { dg-do compile { target c++20 } }

template<class T, bool V> constexpr bool always_true = true;

template<class T, bool V> concept C = always_true<T, V>;

template<bool V, template<class T> requires C<T, V> class TT>
void f();

template<class T> requires C<T, true>
struct A;

int main() {
  f<true, A>();
  f<false, A>(); // { dg-error "no match|constraint mismatch" }
}
