/* Test we do warn about initializing variable with self when -Winit-self is supplied. */
/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized -Winit-self" } */

int f()
{
  int i = i;
  return i;
}
