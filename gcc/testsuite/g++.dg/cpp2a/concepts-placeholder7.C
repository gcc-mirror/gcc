// PR c++/99899
// { dg-do compile { target c++20 } }

template <class T> concept C1 = sizeof(T) > sizeof(int[1]);

template <class>
void f() {
  int x[] = {1,2};
  int y[] = {3};
  C1 auto [a,b] = x;	// { dg-error "structured binding declaration cannot have constrained 'auto' type 'auto \\\[requires ::C1<<placeholder>, >\\\]'" }
  C1 auto [c] = y;	// { dg-error "structured binding declaration cannot have constrained 'auto' type 'auto \\\[requires ::C1<<placeholder>, >\\\]'" }
			// { dg-error "constraints" "" { target *-*-* } .-1 }
}

template <class T>
void g() {
  T x[] = {1,2};
  T y[] = {3};
  C1 auto [a,b] = x;	// { dg-error "structured binding declaration cannot have constrained 'auto' type 'auto \\\[requires ::C1<<placeholder>, >\\\]'" }
  C1 auto [c] = y;	// { dg-error "structured binding declaration cannot have constrained 'auto' type 'auto \\\[requires ::C1<<placeholder>, >\\\]'" }
			// { dg-error "constraints" "" { target *-*-* } .-1 }
}
template void g<int>();


template <class... Ts> concept C2 = sizeof...(Ts) > 1;

struct S { int a, b; } s;

template <class T>
void h() {
  const C2<T> auto& [a, b] = s;	// { dg-error "structured binding declaration cannot have constrained 'auto' type 'const auto \\\[requires ::C2<<placeholder>, >\\\]'" }
}
template void h<int>();
