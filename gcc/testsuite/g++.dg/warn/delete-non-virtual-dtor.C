// { dg-options "-std=gnu++0x -Wdelete-non-virtual-dtor" }
// { dg-do compile }

struct polyBase { virtual void f(); };

void f(polyBase* p, polyBase* arr)
{
  polyBase pb;
  delete p;      // { dg-warning "non-virtual destructor might" }
  delete [] arr;
}

struct polyDerived : polyBase { };

void f(polyDerived* p, polyDerived* arr)
{
  polyDerived pd;
  delete p;      // { dg-warning "non-virtual destructor might" }
  delete [] arr;
}

struct absDerived : polyBase { virtual void g() = 0; };

void f(absDerived* p, absDerived* arr)
{
  delete p;      // { dg-warning "non-virtual destructor will" }
  delete [] arr;
}

struct finalDerived final : polyBase { };

void f(finalDerived* p, finalDerived* arr)
{
  finalDerived fd;
  delete p;      // no error for final classes
  delete [] arr;
}

struct safeBase { virtual ~safeBase(); };
struct safeDerived : safeBase { virtual void f(); };

void f(safeDerived* p, safeDerived* arr)
{
  safeDerived sd;
  delete p;      // no error because base has virtual dtor
  delete [] arr;
}

struct polyBaseNonTrivial { ~polyBaseNonTrivial(); virtual void f(); };

void f(polyBaseNonTrivial* p, polyBaseNonTrivial* arr)
{
  polyBaseNonTrivial pbnt;
  delete p;      // { dg-warning "non-virtual destructor might" }
  delete [] arr;
}

struct polyDerivedNT : polyBaseNonTrivial { ~polyDerivedNT(); };

void f(polyDerivedNT* p, polyDerivedNT* arr)
{
  polyDerivedNT pdnt;
  delete p;      // { dg-warning "non-virtual destructor might" }
  delete [] arr;
}

