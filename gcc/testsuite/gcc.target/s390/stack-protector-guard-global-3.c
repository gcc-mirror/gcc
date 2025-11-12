/* { dg-do compile } */
/* { dg-options "-O2 -fstack-protector-all -mstack-protector-guard=global" } */
/* { dg-final { scan-assembler-times {\tlarl\t%r[0-9]+,__stack_chk_guard\n} 4 } } */
/* { dg-final { scan-assembler-not {\n1:\n} } } */

#include "stack-protector-guard-global-1.c"
