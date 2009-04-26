// { dg-do compile }
// { dg-options "-O2 -fstrict-aliasing -fdump-tree-optimized" }

// Test that placement new does not introduce an unnecessary memory
// barrier.
// See PR 29286.

typedef __SIZE_TYPE__ size_t;

inline void* operator new(size_t, void* __p) throw() { return __p; }

template <class T, int D>
class Vector
{
public:
   Vector()
   {
     for (int i = 0; i < D; ++i)
        new (&x_m[i]) T();
   }
   T& operator[](int i) { return x_m[i]; }

private:
   T x_m[D];
};

void foo(Vector<float, 3> *m)
{
  Vector<float, 3> v;
  v[0] = 1.0;
  v[1] = 2.0;
  v[3] = 3.0;
  *m = v;
}

// { dg-final { scan-tree-dump-times "= 0\.0" 1 "optimized" } }
// { dg-final { cleanup-tree-dump "optimized" } }
