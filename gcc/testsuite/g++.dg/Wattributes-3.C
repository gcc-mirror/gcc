// PR c++/83394 - always_inline vs. noinline no longer diagnosed
// { dg-do compile }
// { dg-options "-Wattributes" }

#define ATTR(list) __attribute__ (list)

struct A
{
  ATTR ((__noinline__)) operator int ();
};

ATTR ((__always_inline__))
A::operator int ()            // { dg-warning "ignoring attribute .always_inline. because it conflicts with attribute .noinline." }
{
  return 0;
}


struct B
{
  operator char () const;
  ATTR ((__always_inline__)) operator int () const;
};

B::operator char () const { return 0; }

ATTR ((__noinline__))
B::operator int () const      // { dg-warning "ignoring attribute .noinline. because it conflicts with attribute .always_inline." }
// { dg-warning "function might not be inlinable unless also declared .inline." "" { target *-*-* } .-1 }

{
  return 0;
}


struct C
{
  operator char ();
  ATTR ((__always_inline__)) operator short ();
  operator int ();
  ATTR ((__noinline__)) operator long ();
};

C::operator char () { return 0; }

ATTR ((__noinline__))
C::operator short ()           // { dg-warning "ignoring attribute .noinline. because it conflicts with attribute .always_inline." }
// { dg-warning "function might not be inlinable unless also declared .inline." "" { target *-*-* } .-1 }
{ return 0; }

inline ATTR ((__noinline__))
C::operator int ()
{ return 0; }


ATTR ((__always_inline__))
C::operator long ()           // { dg-warning "ignoring attribute .always_inline. because it conflicts with attribute .noinline." }
{ return 0; }


struct D
{
  int foo ();
  int foo (int);
  int ATTR ((const)) foo (int, int);
  int ATTR ((pure)) foo (int, int, int);

  int ATTR ((const)) foo (int, int, int, int);

  int foo (int, int, int, int, int);
};

int ATTR ((const))
D::foo ()
{ return 0; }

int ATTR ((pure))
D::foo (int)
{ return 0; }

int ATTR ((pure))
D::foo (int, int)             // { dg-warning "ignoring attribute .pure. because it conflicts with attribute .const." }
{ return 0; }

int ATTR ((const))
D::foo (int, int, int)        // { dg-warning "ignoring attribute .const. because it conflicts with attribute .pure." }
{ return 0; }

int
D::foo (int, int, int, int) { return 0; }

int ATTR ((const))
D::foo (int, int, int, int, int) { return 0; }
