// PR c++/79626

template <class a, class> struct b
{ b(); b(b &); b(b< a, a >); };  // { dg-error "invalid constructor" }
int c(b< int, int >(b< int, int >());  // { dg-error "" }
