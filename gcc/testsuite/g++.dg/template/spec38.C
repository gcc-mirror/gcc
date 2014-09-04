// PR ipa/61659

// { dg-final { scan-assembler "_Z1fIiEvPT_" } }

template <typename T> inline void f (T *);
template <> void f (int *) { }
