/* PR c/39323 */
/* { dg-do compile { target *-*-linux* *-*-gnu* *-*-uclinux* } } */

typedef int __attribute__ ((aligned(1 << 28))) int28;
int28 foo = 20;

/* { dg-final { scan-assembler ".align\[ \t\]+(268435456|28)\[ \t\]*\n" } } */
