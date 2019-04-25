// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 Apr 2005 <nathan@codesourcery.com>
// Re-purposed to check for re-rurgesnce of PR 89693 in 2019.

// { dg-do compile }
// { dg-options "-O3 -fno-ipa-icf-functions" }

// Origin: yanliu@ca.ibm.com
//         nathan@codesourcery.com

struct A {
  virtual void One ();
};
struct B  {
  virtual B *Two ();
  virtual B &Three ();
};

struct C : A, B
{
  virtual C *Two ();
  virtual C &Three ();
};
void A::One () {}
B *B::Two()    {return this;}
B &B::Three()    {return *this;}
C *C::Two ()   {return 0;}
C &C::Three ()   {return *(C *)0;}

B *Foo (B *b)
{
  return b->Two ();
}

B &Bar (B *b)
{
  return b->Three ();
}

int main ()
{
  C c;

  /* We should not adjust a null pointer.  */
  if (Foo (&c))
    return 1;
  /* But we should adjust a (bogus) null reference.  */
  if (!&Bar (&c))
    return 2;

  return 0;
}
