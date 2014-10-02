/* { dg-do run } */ 
/* { dg-options "-O2" } */

void link_error (void);

struct A
{
  int a;
  int b;
};

struct A a;
const int B = 42;

void foo (int i)
{
  if (i > 10)
    a.a = 42;
  else
    {
      a.b = 21;
      a.a = a.b + 21;
    }

  /* This should be folded to 'if (0)' as a.a and B are both 42.  */
  if (a.a != B)
    link_error ();
}

int
main ()
{
  foo (3);
  return 0;
}
