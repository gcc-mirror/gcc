/* PR c/67338 */
/* { dg-do compile { target *-*-linux* *-*-gnu* *-*-uclinux* } } */

struct S { __attribute__((aligned (1 << 28))) double a; };
