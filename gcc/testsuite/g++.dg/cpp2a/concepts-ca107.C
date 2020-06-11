// NB comments CA107/US117
// { dg-do compile { target concepts } }

template <unsigned N> constexpr bool Atomic = true;
template <unsigned N> concept C = Atomic<N>;
template <unsigned N> concept Add1 = C<N + 1>;
template <unsigned N> concept AddOne = C<N + 1>;
template <unsigned M> void f()
  requires Add1<2 * M>;
template <unsigned M> int f()
  requires AddOne<2 * M> && true;

int x = f<0>(); // OK, the atomic constraints from concept C in both fs are Atomic<N>
		// with mapping similar to N -> 2 * M + 1

template <unsigned N> struct WrapN;
template <unsigned N> using Add1Ty = WrapN<N + 1>;
template <unsigned N> using AddOneTy = WrapN<N + 1>;
template <unsigned M> void g(Add1Ty<2 * M> *);
template <unsigned M> void g(AddOneTy<2 * M> *);

void h() {
  g<0>(nullptr); // OK, there is only one g
}
template <unsigned N> void f2()
  requires Add1<2 * N>;
template <unsigned N> int f2()
  requires Add1<N * 2> && true;
void h2() {
  f2<0>(); // { dg-error "ambiguous" } ill-formed, no diagnostic required:
	   // required determination of subsumption between atomic constraints that are
	   // functionally equivalent but not equivalent
}
