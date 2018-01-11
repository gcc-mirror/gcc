/* { dg-do compile } */
/* { dg-options "-march=armv8.3-a+fp16fml" } */

#include "fp16_fmul_lane_low.h"

/* { dg-final { scan-assembler-times "fmlal\\tv\[0-9\]+\.2s, v\[0-9\]+\.2h, v\[0-9\]+\.h\\\[0\\\]" 1 } } */
/* { dg-final { scan-assembler-times "fmlsl\\tv\[0-9\]+\.2s, v\[0-9\]+\.2h, v\[0-9\]+\.h\\\[0\\\]" 1 } } */
/* { dg-final { scan-assembler-times "fmlal\\tv\[0-9\]+\.2s, v\[0-9\]+\.2h, v\[0-9\]+\.h\\\[6\\\]" 1 } } */
/* { dg-final { scan-assembler-times "fmlsl\\tv\[0-9\]+\.2s, v\[0-9\]+\.2h, v\[0-9\]+\.h\\\[6\\\]" 1 } } */
/* { dg-final { scan-assembler-times "fmlal\\tv\[0-9\]+\.4s, v\[0-9\]+\.4h, v\[0-9\]+\.h\\\[1\\\]" 1 } } */
/* { dg-final { scan-assembler-times "fmlsl\\tv\[0-9\]+\.4s, v\[0-9\]+\.4h, v\[0-9\]+\.h\\\[1\\\]" 1 } } */
/* { dg-final { scan-assembler-times "fmlal\\tv\[0-9\]+\.4s, v\[0-9\]+\.4h, v\[0-9\]+\.h\\\[7\\\]" 1 } } */
/* { dg-final { scan-assembler-times "fmlsl\\tv\[0-9\]+\.4s, v\[0-9\]+\.4h, v\[0-9\]+\.h\\\[7\\\]" 1 } } */
