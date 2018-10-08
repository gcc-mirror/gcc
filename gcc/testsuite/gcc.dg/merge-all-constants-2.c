/* { dg-do compile } */
/* { dg-require-effective-target string_merging } */
/* { dg-options "-w -O2 -fmerge-all-constants" } */

const char str1[36] = "0123456789abcdefghijklmnopqrstuvwxyz";
const char str2[37] = "0123456789abcdefghijklmnopqrstuvwxyz";
const char str3[10] = "0123456789abcdefghijklmnopqrstuvwxyz";

/* { dg-final { scan-assembler-not "\\.rodata\[\n\r\]" } } */
