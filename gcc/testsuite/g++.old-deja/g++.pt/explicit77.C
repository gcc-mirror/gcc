// Build don't link:

template <int I, int J, int K>
struct S {};

template <int I, int J>
struct S<I, J, 2> : public S<I, I, I>, S<J, J, J> {};

template <int I, int J, int K>
void f(S<I, J, K>, S<I, I, I>);

void g() {
  S<0, 0, 0> s0;
  S<0, 1, 2> s2;
  
  f<0>(s0, s2);
  f(s0, s2); // ERROR - no matching function
}
