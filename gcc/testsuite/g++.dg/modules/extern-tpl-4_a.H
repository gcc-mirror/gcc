// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

template <typename T> inline void ha() {}
extern template void ha<int>();
extern template void ha<bool>();
template void ha<char>();

template <typename T> void hb() {}
extern template void hb<int>();
extern template void hb<bool>();
template void hb<char>();

template <typename T> inline int hc = 123;
extern template int hc<int>;
extern template int hc<bool>;
template int hc<char>;

template <typename T> int hd = 123;
extern template int hd<int>;
extern template int hd<bool>;
template int hd<char>;
