/* { dg-do run } */
/* { dg-options "-g" } */

int i;
static int f(int) __attribute ((noinline));
static int f(int x)
{
  return i;
}

int main()
{
  return f(42);
}

/* { dg-final { gdb-test 8 "sizeof (x)" "sizeof (int)" } } */
