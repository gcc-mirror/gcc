/* PR tree-optimization/97424.  */

#include <stdint.h>

static inline uint32_t
_dl_hwcaps_subdirs_build_bitmask (int subdirs, int active)
{
  /* Leading subdirectories that are not active.  */
  int inactive = subdirs - active;
  if (inactive == 32)
    return 0;

  uint32_t mask;
  if (subdirs != 32)
    mask = (1 << subdirs) - 1; /* { dg-message "shift by count \\('33'\\) >= precision of type \\('\[0-9\]+'\\)" } */
  else
    mask = -1;
  return mask ^ ((1U << inactive) - 1); /* { dg-message "shift by negative count \\('-1'\\)" } */
}

void f1 (int);

void
f2 (void)
{
  f1 (_dl_hwcaps_subdirs_build_bitmask (1, 2));
  f1 (_dl_hwcaps_subdirs_build_bitmask (33, 31));
}

static int __attribute__((noinline)) op3 (int op, int c) { return op << c; } /* { dg-message "shift by negative count \\('-1'\\)" } */
int test_3 (void) { return op3 (1, -1); }

static int __attribute__((noinline)) op4 (int op, int c) { return op << c; }
int test_4 (void) { return op4 (1, 0); }
