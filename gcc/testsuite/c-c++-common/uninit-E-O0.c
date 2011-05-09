/* Test we do warn about initializing variable with self when -Winit-self is supplied. */
/* { dg-do compile } */
/* { dg-options "-Wuninitialized -Winit-self" } */

int f()
{
  int i = i; /* { dg-warning "i" "uninitialized variable warning" }  */
  return i;
}
