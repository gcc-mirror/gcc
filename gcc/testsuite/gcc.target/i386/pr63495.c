/* PR c/63495 */
/* { dg-do compile { target { i?86-*-linux* x86_64-*-linux* } } } */
/* { dg-options "-std=gnu11" } */

struct __attribute__ ((aligned (8))) S { char c; };
_Static_assert (_Alignof (struct S) >= 8, "wrong alignment");
