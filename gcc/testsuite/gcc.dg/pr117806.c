/* { dg-do compile } */
/* { dg-excess-errors "warnings about ignored 'packed' attribute" { target default_packed } } */
/* { dg-options "-std=c23" } */

struct Test {
  double D __attribute__((packed,aligned(4)));
} x;
struct Test {
  double D __attribute__((packed,aligned(4)));
} x;
struct Test {
  double D __attribute__((packed,aligned(4)));
} x;

