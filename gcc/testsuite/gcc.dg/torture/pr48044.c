/* PR middle-end/48044 */
/* { dg-do compile } */
/* { dg-require-alias "" } */

int a __asm__ ("b") = 0;
extern int c __asm__ ("a") __attribute__ ((alias ("b")));
extern int d __attribute__ ((weak, alias ("a")));
