/* { dg-do compile } */
/* { dg-options "-O2 -mforce-indirect-call -fPIC" } */
/* { dg-require-effective-target fpic } */
/* { dg-final { scan-assembler-times "(?:call|jmp)\[ \\t\]+\\*" 3 } } */

#include "force-indirect-call-1.c"
