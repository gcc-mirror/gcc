void ambig()
{
  struct A {};
  struct B : A {};
  struct C : A {};
  struct D : B, C {};

  D d;
  A* ap = static_cast<B*> (&d);
  D* db = static_cast<D*> (ap); // { dg-error "" }
  
  D& dr1 = static_cast<D&> (*ap); // { dg-error "" }
  
  A& ar = static_cast<C&> (d);
  D& dr = static_cast<D&> (ar);  // { dg-error "" }
}

