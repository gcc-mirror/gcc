// { dg-do compile { target c++2a } }

template <class T> struct A { static const int x = 42; };

template <class Ta> concept A42 = A<Ta>::x == 42;
template <class Tv> concept Void = __is_same_as(Tv, void);
template <class Tb, class Ub> concept A42b = Void<Tb> || A42<Ub>;
template <class Tc> concept R42c = A42b<Tc, Tc&>;

static_assert (R42c<void>);
