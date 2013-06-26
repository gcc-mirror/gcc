/* PR c/39323 */
/* { dg-do compile { target *-*-linux* *-*-gnu* } } */

int bar __attribute__ ((aligned(1 << 28))) =  20;

/* { dg-final { scan-assembler ".align\[ \t\]+(268435456|28)\[ \t\]*\n" } } */
