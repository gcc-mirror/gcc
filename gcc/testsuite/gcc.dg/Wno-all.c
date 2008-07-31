/* PR 30437: Test negative of -Wall
   Don't change this without changing Wall.c as well.  */
/* { dg-do compile } */
/* { dg-options "-Wall -Wno-all" } */

void foo(int a)
{
  5 * (a == 1) | (a == 2);  /* { dg-bogus "no effect" "no effect" } */
}

