// { dg-do compile { target c++20 } }

template <class T, class U> concept same_as = __is_same(T, U);

// this constrained placeholder type should get resolved at parse time
template <class T> same_as<bool> auto x = true;

template <auto V> same_as<int> auto y = V; // { dg-error "constraint" }

template <class>
struct A {
  template <auto V> static inline same_as<int> auto z = V; // { dg-error "constraint" }
};

int main() {
  x<int>;          // { dg-bogus "" }
  y<0>;            // { dg-bogus "" }
  y<'0'>;          // { dg-message "required from here" }
  A<void>::z<0>;   // { dg-bogus "" }
  A<void>::z<'0'>; // { dg-message "required from here" }
}

// unsatisfied placeholder type constraint diagnosed at parse time
template <class T> same_as<int> auto w = true; // { dg-error "constraint" }
