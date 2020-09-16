/* PR middle-end/96295 - -Wmaybe-uninitialized warning for range operator
   with reference to an empty struct
   { dg-do compile }
   { dg-options "-Wall" }
   { dg-require-effective-target c++11 } */

struct I
{
  bool operator!= (const I&) const;
  void* operator* () const;
  I& operator++ ();
};

struct A
{
  I begin () const { return I (); }
  I end () const { return I (); }
};

void f (void)
{
  for (void *p : A ())   // { dg-bogus "\\\[-Wmaybe-uninitialized" }
    {
      (void)p;
    }
}
