/* { dg-do compile } */
/* { dg-additional-options "-O1 -fnon-call-exceptions" } */

struct A
{
  A (): a (0), b (0), c (0)
  {
  };
  ~A ();
  int a, b, c;
};

struct B
{
  B ();
  A a1;
  A a2;
};

B::B ()
{
}

