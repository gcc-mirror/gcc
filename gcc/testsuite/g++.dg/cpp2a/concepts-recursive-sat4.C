// PR c++/96840
// { dg-do compile { target c++20 } }

template <class T, class U> concept C = requires(T t, U u) { t * u; };
// { dg-message "required for the satisfaction of 'C<T, Rep>' .with T = Int<int>; Rep = int." "" { target *-*-* } .-1 }
// { dg-error "depends on itself" "" { target *-*-* } .-2 }

template <class Rep> struct Int {
  template <class T> requires C<T, Rep> friend void operator*(T, Int) { }
  template <class T> requires C<T, Rep> friend void operator*(Int, T) { }
};

void f() { 0 * Int<int>{}; }
