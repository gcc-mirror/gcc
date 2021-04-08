// PR c++/91849

struct A { operator float(); };

void
g ()
{
  float f = 1.f;
  int &r = f;			// { dg-error "float" }
  int &r2 = A();		// { dg-error "float" }
}

void
g2 ()
{
  int &r = 1.f;			// { dg-error "float" }
}
