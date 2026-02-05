/* PR c/123882 */
/* { dg-do compile } */
/* { dg-options "-std=c23" } */

typedef int T;
void foo (unsigned long, T[]);
void foo (unsigned long x, T[restrict x]);
