// Build don't link: 
// GROUPS passed visibility

class a {

private:
  a (int i);// ERROR - .*

public:
  a ();
};

void test ()
{
  a *ap = new a;
  a *ap2 = new a (3);// ERROR - .*
}
