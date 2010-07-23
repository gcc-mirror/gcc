/* Test for ICE with C99-conforming excess precision and -msse.  PR
   39592.  */
/* { dg-do compile } */
/* { dg-options "-ansi -msse" } */

double
foo (unsigned long var)
{
  return var;
}
