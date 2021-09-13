// PR c++/98570
// { dg-do compile { target c++11 } }

template <int> struct b { enum { c }; };
template <typename> using i = b<0>;

template <int> struct d { };
template <typename l> d<i<l>::c> m() { }
template <typename n> d<i<n*>::c> m() { }
