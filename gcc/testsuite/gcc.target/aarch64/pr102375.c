/* PR target/102375 */
/* { dg-do compile } */

void calculate(void) __attribute__ ((target ("+sve, +sve2")));
