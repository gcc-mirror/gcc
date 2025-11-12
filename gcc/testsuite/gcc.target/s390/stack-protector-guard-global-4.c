/* { dg-do compile } */
/* { dg-options "-O2 -fstack-protector-all -mstack-protector-guard=global -fPIC" } */
/* { dg-final { scan-assembler-times {\t(larl|lg?rl)\t%r[0-9]+,__stack_chk_guard@GOTENT\n} 4 } } */
/* { dg-final { scan-assembler-not {\n1:\n} } } */

#include "stack-protector-guard-global-1.c"
