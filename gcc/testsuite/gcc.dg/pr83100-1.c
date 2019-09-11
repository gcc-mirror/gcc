/* PR target/83100 */
/* { dg-do compile { target *-*-linux* *-*-gnu* *-*-uclinux* } } */
/* { dg-options "-O2 -fcommon -fdata-sections" } */

const int a;

/* { dg-final { scan-assembler "comm" } } */
