/* Test we do not warn about initializing variable with address of self in the initialization. */
/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

void *f()
{
  void *i = &i;
  return i;
}
