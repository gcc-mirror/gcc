// { dg-lto-do assemble }

template<class T, unsigned long l>
inline unsigned long foo (T (&)[l]) { return l; }

struct S { char *s[4]; S (); };

S::S () { typedef int T[foo (s) == 4 ? 1 : -1]; }
