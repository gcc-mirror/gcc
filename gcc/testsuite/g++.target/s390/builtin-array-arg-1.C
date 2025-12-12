/* { dg-do compile } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { scan-assembler-times {\tlarl\t(%r1),.LANCHOR0\n\tvllezf\t%v24,0\(\1\)\n\tbr\t%r14} 2 } } */
/* { dg-final { scan-assembler-times {\tlarl\t(%r1),.LANCHOR0\n\tvl\t%v24,4\(\1\)\n\tbr\t%r14} 1 } } */

/* Test C++ semantics w.r.t. array arguments.  */

#include "../../gcc.target/s390/builtin-array-arg-1.c"
