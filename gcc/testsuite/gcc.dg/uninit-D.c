/* Test we do not warn about initializing variable with self. */
/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

int f()
{
  int i = i;
  return i;
}
