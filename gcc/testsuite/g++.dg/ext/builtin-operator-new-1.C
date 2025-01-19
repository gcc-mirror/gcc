// { dg-do compile }
// { dg-options "-O2 -fdump-tree-gimple -fdump-tree-optimized" }
// { dg-final { scan-tree-dump " = operator new \\\(" "gimple" } }
// { dg-final { scan-tree-dump "operator delete \\\(" "gimple" } }
// { dg-final { scan-tree-dump-not "operator new \\\(" "optimized" } }
// { dg-final { scan-tree-dump-not "operator delete \\\(" "optimized" } }

#include <new>

#if __has_builtin (__builtin_operator_new) != 201802L
#error "Unexpected value of __has_builtin (__builtin_operator_new)"
#endif
#if __has_builtin (__builtin_operator_delete) != 201802L
#error "Unexpected value of __has_builtin (__builtin_operator_delete)"
#endif

void
foo ()
{
  void *a = __builtin_operator_new (32);
  __builtin_operator_delete (a);
#if __cpp_sized_deallocation
  a = __builtin_operator_new (32);
  __builtin_operator_delete (a, 32);
#endif
#if __cpp_aligned_new
  void *b = __builtin_operator_new (32, std::align_val_t(32));
  __builtin_operator_delete (b, std::align_val_t(32));
#if __cpp_sized_deallocation
  b = __builtin_operator_new (32, std::align_val_t(32));
  __builtin_operator_delete (b, 32, std::align_val_t(32));
#endif
#endif
  void *c = __builtin_operator_new (32, std::nothrow);
  __builtin_operator_delete (c, std::nothrow);
#if __cpp_aligned_new
  void *d = __builtin_operator_new (32, std::align_val_t(32), std::nothrow);
  __builtin_operator_delete (d, std::align_val_t(32), std::nothrow);
#endif
  void *e = __builtin_operator_new (1.f);
  __builtin_operator_delete (e);
}

template <int N>
void
bar ()
{
  void *a = __builtin_operator_new (32);
  __builtin_operator_delete (a);
#if __cpp_sized_deallocation
  a = __builtin_operator_new (32);
  __builtin_operator_delete (a, 32);
#endif
#if __cpp_aligned_new
  void *b = __builtin_operator_new (32, std::align_val_t(32));
  __builtin_operator_delete (b, std::align_val_t(32));
#if __cpp_sized_deallocation
  b = __builtin_operator_new (32, std::align_val_t(32));
  __builtin_operator_delete (b, 32, std::align_val_t(32));
#endif
#endif
  void *c = __builtin_operator_new (32, std::nothrow);
  __builtin_operator_delete (c, std::nothrow);
#if __cpp_aligned_new
  void *d = __builtin_operator_new (32, std::align_val_t(32), std::nothrow);
  __builtin_operator_delete (d, std::align_val_t(32), std::nothrow);
#endif
  void *e = __builtin_operator_new (1.f);
  __builtin_operator_delete (e);
}

template <typename T, typename U, typename V, typename W>
void
baz (T sz, V v, W w)
{
  U a = __builtin_operator_new (sz);
  __builtin_operator_delete (a);
#if __cpp_sized_deallocation
  a = __builtin_operator_new (sz);
  __builtin_operator_delete (a, sz);
#endif
#if __cpp_aligned_new
  U b = __builtin_operator_new (sz, std::align_val_t(sz));
  __builtin_operator_delete (b, std::align_val_t(sz));
#if __cpp_sized_deallocation
  b = __builtin_operator_new (sz, std::align_val_t(sz));
  __builtin_operator_delete (b, sz, std::align_val_t(sz));
#endif
#endif
  U c = __builtin_operator_new (sz, v);
  __builtin_operator_delete (c, v);
#if __cpp_aligned_new
  U d = __builtin_operator_new (sz, std::align_val_t(sz), v);
  __builtin_operator_delete (d, std::align_val_t(sz), v);
#endif
  U e = __builtin_operator_new (w);
  __builtin_operator_delete (e);
}

void
qux ()
{
  bar <0> ();
  baz <std::size_t, void *, const std::nothrow_t &, float> (32, std::nothrow,
							    1.f);
}
