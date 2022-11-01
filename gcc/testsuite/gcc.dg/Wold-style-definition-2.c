/* PR c/12466
   Test for not warning about ellipsises with -Wold-style-definition. */

/* Origin: Kelley Cook <kcook@gcc.gnu.org> */
/* { dg-do compile } */
/* { dg-options "-Wold-style-definition" } */

void bar1 ( ... ) {}

void bar2 (int a, ... ) {}
