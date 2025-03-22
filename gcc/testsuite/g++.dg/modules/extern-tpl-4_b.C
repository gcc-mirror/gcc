// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }

export module M;

export template <typename T> inline void ma() {}
extern template void ma<int>();
extern template void ma<bool>();
template void ma<char>();

export template <typename T> void mb() {}
extern template void mb<int>();
extern template void mb<bool>();
template void mb<char>();

export template <typename T> inline int mc = 123;
extern template int mc<int>;
extern template int mc<bool>;
template int mc<char>;

export template <typename T> int md = 123;
extern template int md<int>;
extern template int md<bool>;
template int md<char>;
