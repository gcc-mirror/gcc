/* PR c/71685 */
/* { dg-do compile } */
/* { dg-options "-std=gnu11" } */

extern struct S v, s;
struct S { int t; int p[]; } v = { 4, 0 };
