/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mms-bitfields" } */

struct S { int i[(1LL << 60) - 1]; };
