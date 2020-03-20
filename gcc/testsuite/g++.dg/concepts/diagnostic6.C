// { dg-do compile { target c++2a } }

template<typename T>
  concept C = requires (T t) { t + 0; };
// { dg-message "satisfaction of .C<T>. .with T = typename T::type." "" { target *-*-* } .-1 }

template<typename T>
  concept D = C<T>;
// { dg-message "satisfaction of .D<typename T::type>. .with T = int." "" { target *-*-* } .-1 }

template<typename T>
  concept E = D<typename T::type>;

static_assert(E<int>); // { dg-error "static assertion failed|not a class" }
