#ifndef HAVE_DEFINED_VX_VF_BINARY_H
#define HAVE_DEFINED_VX_VF_BINARY_H

#include <stdint.h>

#define DEF_VX_BINARY_CASE_0(T, OP)                                        \
void                                                                       \
test_vx_binary_case_0 (T * restrict out, T * restrict in, T x, unsigned n) \
{                                                                          \
  for (unsigned i = 0; i < n; i++)                                         \
    out[i] = in[i] OP x;                                                   \
}
#define DEF_VX_BINARY_CASE_0_WRAP(T, OP)         DEF_VX_BINARY_CASE_0(T, OP)
#define RUN_VX_BINARY_CASE_0(out, in, x, n)      test_vx_binary_case_0(out, in, x, n)
#define RUN_VX_BINARY_CASE_0_WRAP(out, in, x, n) RUN_VX_BINARY_CASE_0(out, in, x, n)

#endif
