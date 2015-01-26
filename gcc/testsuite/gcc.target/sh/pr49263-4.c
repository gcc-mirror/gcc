/* Verify that TST #imm, R0 instruction is generated if the constant
   allows it when compiling for -Os.  */
/* { dg-do compile }  */
/* { dg-options "-Os" } */
/* { dg-final { scan-assembler-not "and" } } */
/* { dg-final { scan-assembler-not "extu" } } */
/* { dg-final { scan-assembler-not "exts" } } */
/* { dg-final { scan-assembler-not "shlr" } } */

#include "pr49263.c"
