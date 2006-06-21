/* { dg-do compile } */
/* { dg-options "-w -O2 -fmerge-all-constants" } */

const char str1[36] = "0123456789abcdefghijklmnopqrstuvwxyz";
const char str2[38] = "0123456789abcdefghijklmnopqrstuvwxyz";
const char str3[10] = "0123456789abcdefghijklmnopqrstuvwxyz";

/* { dg-final { scan-assembler-not "\.rodata\.str" } } */
