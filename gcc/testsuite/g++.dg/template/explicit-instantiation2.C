// Bug 10968: implicit instantiation overrides explicit instantiation
// { dg-final { scan-assembler "_Z1fIiET_S0_" } }

template <class T> T f (T t) { return t; }
inline void g () { f (4); }
template int f (int);
