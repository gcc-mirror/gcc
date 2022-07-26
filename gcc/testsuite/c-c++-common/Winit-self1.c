/* PR middle-end/102633 */
/* { dg-do compile } */
/* { dg-options "-Wuninitialized -Wno-init-self" } */

int
fn1 (void)
{
  int i = i;
  return i;
}

int
fn2 ()
{
  const int j = j;
  return j;
}

int
fn3 ()
{
  volatile int k = k;
  return k;
}

int
fn4 ()
{
  const volatile int l = l;
  return l;
}
