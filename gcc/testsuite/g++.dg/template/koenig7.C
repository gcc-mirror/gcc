// PR c++/13549
// We need to do arg-dep lookup for g<T>(j) at instantiation time because
// g<T> is dependent, even though (j) is not; at that point we can find
// g(h).

template <typename T> int g(int);
class h{};
template <typename T> int l(){h j; return g<T>(j);}
template <typename T> int g(const h&);
class j{};
int jj(){return l<j>();}
