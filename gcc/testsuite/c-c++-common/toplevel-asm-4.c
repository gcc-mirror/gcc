/* PR c/41045 */
/* { dg-do compile } */
/* { dg-options "-O0" } */
/* { dg-additional-options "-fno-pie" { target pie } } */

int v[42], w;
void foo (void);

asm ("# %c0: %c1:" :: ":" (foo), ":" (v), ":" (&w));
