#ifndef HAVE_DEFINED_PR121959_H
#define HAVE_DEFINED_PR121959_H

#include <stdint.h>

#define DEF_VWSLL_FUNC_0(WT, NT, IMM)                   \
void                                                    \
test_from_##NT##_to_##WT##_##IMM##_0(WT * restrict res, \
				     NT * restrict a,   \
				     NT * restrict b,   \
				     int n)             \
{                                                       \
  for (int i = 0; i < n; i++)                           \
    {                                                   \
      res[i] = (a[i] - b[i]) << IMM;                    \
    }                                                   \
}
#define DEF_VWSLL_FUNC_0_WRAP(WT, NT, IMM) DEF_VWSLL_FUNC_0(WT, NT, IMM)
#define RUN_VWSLL_FUNC_0(WT, NT, IMM, res, a, b, n) \
  test_from_##NT##_to_##WT##_##IMM##_0(res, a, b, n)
#define RUN_VWSLL_FUNC_0_WRAP(WT, NT, IMM, res, a, b, n) \
  RUN_VWSLL_FUNC_0(WT, NT, IMM, res, a, b, n)

#endif
