// { dg-do assemble  }
// GROUPS passed visibility

class a {

private:
  a (int i);// { dg-error "" } .*

public:
  a ();
};

void test ()
{
  a *ap = new a;
  a *ap2 = new a (3);// { dg-error "" } .*
}
