/* { dg-do compile } */
/* { dg-options "-O1 -ftree-vectorize -fnon-call-exceptions" } */

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

/* { dg-final { cleanup-tree-dump "vect" } } */
