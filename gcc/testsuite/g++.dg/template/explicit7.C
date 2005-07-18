// PR c++/22263
// { dg-do link }

template <class T> struct S { T foo (); T bar (); };
template <class T> T S<T>::foo () { return bar (); }
template struct S<int>;
template <class T> T S<T>::bar () { return T (); }

#if !__GXX_WEAK__
template int S<int>::bar ();
#endif

int main () { return S<int>().foo (); }
