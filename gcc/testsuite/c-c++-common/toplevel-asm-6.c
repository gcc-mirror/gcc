/* PR c/41045 */
/* { dg-do compile } */
/* { dg-options "-O0" } */
/* { dg-additional-options "-fPIC" { target fpic } } */

extern int v[42], w;
void foo (void);

asm ("# %cc0: %cc1:" :: ":" (foo), ":" (v), ":" (&w));
