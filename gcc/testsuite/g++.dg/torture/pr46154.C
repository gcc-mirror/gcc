/* { dg-do compile } */
/* { dg-options "-fipa-cp-clone" } */

struct S
{
  virtual int foo ()
    {
      return foo () == 0;
    }
  virtual void baz ();
};

void A ()
{
  S s;
  s.foo ();
}
