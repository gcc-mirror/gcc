/* PR middle-end/102633 */
/* { dg-do compile } */
/* { dg-options "-Wuninitialized -Winit-self" } */

int
fn1 (void)
{
  int i = i; /* { dg-warning "used uninitialized" } */
  return i;
}

int
fn2 ()
{
  const int j = j; /* { dg-warning "used uninitialized" } */
  return j;
}

int
fn3 ()
{
  volatile int k = k; /* { dg-warning "used uninitialized" } */
  return k;
}

int
fn4 ()
{
  const volatile int l = l; /* { dg-warning "used uninitialized" } */
  return l;
}
