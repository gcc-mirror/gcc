// PR c++40342

template <typename T1, typename T2> int f(T1 *, const T2 *); // { dg-message "" }
template <typename T1, typename T2> int f(const T1 *, T2 *); // { dg-message "" }

int (*p)(const int *, const int *) = f; // { dg-error "ambiguous" }
