/* PR middle-end/21781.  */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

int foo(void) { if (.0e200000000 == 0 ) return 1; }
