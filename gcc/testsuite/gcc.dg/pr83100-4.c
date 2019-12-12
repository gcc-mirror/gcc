/* PR target/83100 */
/* { dg-do compile { target *-*-linux* *-*-gnu* *-*-uclinux* } } */
/* { dg-options "-O2 -fno-common -fdata-sections" } */

const int a;

/* { dg-final { scan-assembler "rodata.a" { target i?86-*-* x86_64-*-* } } } */
