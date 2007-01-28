typedef int C;
typedef double D;

void
f ()
{
  C o;

  o.D::~C (); // { dg-error "" }
}
