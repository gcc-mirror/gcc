// PR c++/90997

template<class> void f ()
{ __builtin_memset (0, 0, int(0.)); }
