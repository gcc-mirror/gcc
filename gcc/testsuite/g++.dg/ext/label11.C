// PR c++/38725
// { dg-do compile }
// { dg-options "" }

struct A {};
struct B : virtual A {};
int vi;
void *vp;

void
f1 (int i)
{
  goto *i;
}

void
f2 (B b)
{
  goto *b;	// { dg-error "cannot convert" }
}

template <typename T>
void
f3 (T i)
{
  goto *i;
}

void
f3a ()
{
  f3 (vi);
}

template <typename T>
void
f4 (T i)
{
  goto *i;
}

void
f4a ()
{
  f4 (vp);
}
