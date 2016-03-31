/* PR c/70297 */
/* { dg-do compile } */
/* { dg-options "" } */

#define N 64

typedef int T;
typedef int T __attribute__((aligned (N)));
typedef int T __attribute__((aligned (N * 2)));
typedef int T __attribute__((aligned (N)));
typedef int T;

_Static_assert (_Alignof (T) == N * 2, "N * 2");
