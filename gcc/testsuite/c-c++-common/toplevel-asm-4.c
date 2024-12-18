/* PR c/41045 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

extern int v[42], w;
int x[42], y;
void foo (void);
void bar (void) {}

asm ("# %cc0: %cc1: %cc2: %cc3 %cc4 %cc5" :: ":" (foo), ":" (v), ":" (&w), "-i" (bar), "-s" (x), "-s" (&y));
