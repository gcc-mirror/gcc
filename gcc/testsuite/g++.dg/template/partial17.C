// [temp.spec.partial.match]/3

template <int I, int J> struct A;
template <int I> struct A<I+5, I*2> {};     // { dg-error "not deducible" }

template <int I> struct A<I, I> {};         // OK

template <int I, int J, int K> struct B;
template <int I> struct B<I, I*2, I> {};    // OK
template <int I> struct B<I, I*2, 2> { typedef int type; };    // OK

B<1, 2, 1> b1;
B<1, 2, 2>::type b2;
B<1, 2, 3> b3; // { dg-error "incomplete" }
