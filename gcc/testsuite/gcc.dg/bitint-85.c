/* PR c/113740 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23" } */

struct S { unsigned _BitInt(32) : 0; };
