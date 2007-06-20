/* { dg-do compile { target { { i?86-*-linux* x86_64-*-linux* } || { i?86-*-* x86_64-*-darwin* } } } } */
/* { dg-options "-m64 -mcmodel=medium" } */
/* { dg-final { scan-assembler "8589934592|8589934588" } } */
int bigarray[2147483647];
