// PR c++/10200

template<class Tp> inline void end(Tp) { }

template <typename T> bool tnegative(const T& t) { return t.end < 0; }
