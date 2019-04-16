// PR c++/89876
// { dg-do compile { target c++11 } }
// { dg-prune-output "sorry" }

template <typename T>
T f (T, char*);

template <typename T>
decltype (f (T (), "")) g (T) { return ""; } // { dg-error "invalid conversion" }

void h () { g (0); }
