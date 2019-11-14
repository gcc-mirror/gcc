// { dg-require-effective-target c++14 }

template <typename> void a();
template <typename> struct b;
template <bool> using c = int;
template <typename d, typename e = decltype(a<d>)> using f = e;
template <typename e> using g = f<e>;
template <typename h> c<b<g<h>>::i> j;
