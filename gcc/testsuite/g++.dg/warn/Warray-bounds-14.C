/* PR middle-end/97595 - bogus -Wstringop-overflow due to DECL_SIZE_UNIT
   underreporting field size
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

struct A { char a[32]; };
struct B: virtual A { };
struct C: B { };

struct D
{
  B &b;
  D (B&);
};

D::D (B &b): b (b) { }        // { dg-bogus "-Warray-bounds" }

void f (void*);

void g ()
{
  C c;
  D d (c);
  f (&d);
}
