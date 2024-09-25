// { dg-additional-options "-mavx2 -O3" { target avx2_runtime } }
// { dg-additional-sources pr95401a.cc linkonly }

extern int var_9;
extern unsigned var_14;
extern int arr_16[];
#include <algorithm>
void test() {
  for (short a = 0; a < (short)var_9; a += 12140)
    for (short b = 0; b < 8; b++)
      if (std::max(var_14, 1U))
        arr_16[a + b] = 0;
}
