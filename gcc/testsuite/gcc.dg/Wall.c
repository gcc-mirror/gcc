/* PR 30437: Test -Wall 
   Don't change this without changing Wno-all.c as well.  */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

void foo(int a)
{
  5 * (a == 1) | (a == 2);  /* { dg-warning "no effect" "no effect" } */
}
