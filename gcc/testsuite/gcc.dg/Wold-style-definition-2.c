/* PR c/12466
   Test for not warning about ellipsises with -Wold-style-definition. */

/* Origin: Kelley Cook <kcook@gcc.gnu.org> */
/* { dg-do compile } */
/* { dg-options "-Wold-style-definition" } */

void bar1 ( ... ) {} /* { dg-error "ISO C requires a named argument" } */

void bar2 (int a, ... ) {}
