// PR c++/112632
// { dg-do compile { target c++20 } }

template<int N> concept A = N != 0;
template<char C> concept B = A<C>;
template<int N> concept C = B<N>;

static_assert(A<256>);
static_assert(!C<256>);
