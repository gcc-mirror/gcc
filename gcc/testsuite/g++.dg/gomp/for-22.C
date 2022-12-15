// { dg-do compile { target c++17 } }

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

struct A {
  int i;
  template <int I> int& get() { return i; }
};

template<> struct std::tuple_size<A> { static const int value = 3; };
template<int I> struct std::tuple_element<I,A> { using type = int; };

struct B {
  A *begin();
  A *end();
};

void
f1 (B a)
{
  #pragma omp for collapse (2)
  for (auto [i, j, k] : a)			// { dg-error "initializer expression refers to iteration variable 'i'" "" { target *-*-* } .+1 }
    for (int l = i; l < j; l += k)		// { dg-error "condition expression refers to iteration variable 'j'" }
      ;						// { dg-error "increment expression refers to iteration variable 'k'" "" { target *-*-* } .-1 }
}

template <int N>
void
f2 (B a)
{
  #pragma omp for collapse (2)
  for (auto [i, j, k] : a)			// { dg-error "initializer expression refers to iteration variable 'i'" "" { target *-*-* } .-1 }
    for (int l = i; l < j; l += k)		// { dg-error "condition expression refers to iteration variable 'j'" }
      ;						// { dg-error "increment expression refers to iteration variable 'k'" "" { target *-*-* } .-3 }
}

template <typename T>
void
f3 (T a)
{
  #pragma omp for collapse (2)
  for (auto [i, j, k] : a)			// { dg-error "initializer expression refers to iteration variable 'i'" "" { target *-*-* } .-1 }
    for (int l = i; l < j; l += k)		// { dg-error "condition expression refers to iteration variable 'j'" }
      ;						// { dg-error "increment expression refers to iteration variable 'k'" "" { target *-*-* } .-3 }
}

void
test ()
{
  B b;
  f1 (b);
  f2 <0> (b);
  f3 <B> (b);
}
