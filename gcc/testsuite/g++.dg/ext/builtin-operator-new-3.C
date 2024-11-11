// { dg-do run }

#include <new>

void
use (void *p)
{
  int *volatile q = new (p) int;
  *q = 42;
  (*q)++;
  if (*q != 43)
    __builtin_abort ();
}

int
main ()
{
  void *volatile a = __builtin_operator_new (sizeof (int));
  use (a);
  __builtin_operator_delete (a);
#if __cpp_sized_deallocation
  a = __builtin_operator_new (sizeof (int));
  use (a);
  __builtin_operator_delete (a, sizeof (int));
#endif
#if __cpp_aligned_new
  void *volatile b = __builtin_operator_new (sizeof (int), std::align_val_t(alignof (int) * 4));
  use (b);
  __builtin_operator_delete (b, std::align_val_t(alignof (int) * 4));
#if __cpp_sized_deallocation
  b = __builtin_operator_new (sizeof (int), std::align_val_t(alignof (int) * 4));
  use (b);
  __builtin_operator_delete (b, sizeof (int), std::align_val_t(alignof (int) * 4));
#endif
#endif
  void *volatile c = __builtin_operator_new (sizeof (int), std::nothrow);
  use (c);
  __builtin_operator_delete (c, std::nothrow);
#if __cpp_aligned_new
  void *volatile d = __builtin_operator_new (sizeof (int), std::align_val_t(alignof (int) * 4), std::nothrow);
  use (d);
  __builtin_operator_delete (d, std::align_val_t(sizeof (int)), std::nothrow);
#endif
  void *volatile e = __builtin_operator_new (1.f);
  use (e);
  __builtin_operator_delete (e);
}
