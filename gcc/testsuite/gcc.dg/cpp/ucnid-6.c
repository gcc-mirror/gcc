/* { dg-do compile } */
/* { dg-options "-std=c89" } */
#define a b(
#define b(x) q
int a\u00aa);
