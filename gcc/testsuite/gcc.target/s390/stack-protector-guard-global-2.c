/* { dg-do compile } */
/* { dg-options "-O2 -fstack-protector-all -mstack-protector-guard=global -mstack-protector-guard-record -fPIC" } */
/* { dg-final { scan-assembler-times {\n1:\n\t(larl|lg?rl)\t%r[0-9]+,__stack_chk_guard@GOTENT\n} 4 } } */

#include "stack-protector-guard-global-1.c"
