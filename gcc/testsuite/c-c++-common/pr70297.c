/* PR c/70297 */
/* { dg-do compile } */
/* { dg-options "-g" } */

typedef int T;
typedef int T __attribute__((aligned (4)));
struct S {
  T *t;
};
