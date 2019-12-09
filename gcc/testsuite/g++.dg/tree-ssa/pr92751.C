// { dg-do compile }
// { dg-options "-O -fdump-tree-fre1" }

inline void* operator new(__SIZE_TYPE__, void* p) { return p; }
template<int N>
struct Vec {
  Vec(int v) : lo(v), hi(v) {};
  Vec<N/2> lo, hi;
};
template<>
struct Vec<1> {
  Vec(int v) : val(v) {}
  int val;
};

typedef int v4si __attribute__((vector_size(16)));
void foo (v4si *dst)
{
  Vec<4> v(1);
  v4si tem;
  __builtin_memcpy (&tem, &v, sizeof (tem));
  *dst = tem;
}

// FRE should be able to value-number 'tem' to a constant.  */
// { dg-final { scan-tree-dump "\\*dst_\[0-9\]*\\\(D\\\) = { 1, 1, 1, 1 };" "fre1" } }
