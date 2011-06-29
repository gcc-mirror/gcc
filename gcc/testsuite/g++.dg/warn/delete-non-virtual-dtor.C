// { dg-options "-std=gnu++0x -Wdelete-non-virtual-dtor" }
// { dg-do compile }

struct polyBase { virtual void f(); };

void f(polyBase* p, polyBase* arr)
{
  delete p;      // { dg-warning "non-virtual destructor might" }
  delete [] arr;
}

struct polyDerived : polyBase { };

void f(polyDerived* p, polyDerived* arr)
{
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
  delete p;      // no error for final classes
  delete [] arr;
}

struct safeBase { virtual ~safeBase(); };
struct safeDerived : safeBase { virtual void f(); };

void f(safeDerived* p, safeDerived* arr)
{
  delete p;      // no error because base has virtual dtor
  delete [] arr;
}

