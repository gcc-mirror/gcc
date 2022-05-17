/* PR c++/84850 - missing -Wclass-memaccess for a memcpy in a copy ctor
   with a non-trivial member
   { dg-do compile }
   { dg-options "-Wclass-memaccess -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

extern "C" void* memcpy (void*, const void*, size_t);

struct A
{
  const int &r;

  A ();

  A (const A&);

  virtual ~A ();
};

struct C
{
  A a;

  C (const C&);

  C& operator= (const C&);
};

C::C (const C &c)
{
  memcpy (this, &c, sizeof c);    // { dg-warning "\\\[-Wclass-memaccess]" }
}

C& C::operator= (const C &c)
{
  memcpy (this, &c, sizeof c);    // { dg-warning "\\\[-Wclass-memaccess]" }
  return *this;
}
