/* Test that the NRV optimization doesn't cause a1 to change too soon.  This
   is equivalent to c++/19317.  */
/* { dg-do run } */

void abort (void);

struct A
{
  int i[100];
};

struct A a1;

struct A f ()
{
  struct A a2;
  a2.i[0] = 42;
  /* a1.i[0] should still be 0 until we return. */
  if (a1.i[0] != 0)
    abort ();
  return a2;
}

int main()
{
  a1 = f();
  return 0;
}
