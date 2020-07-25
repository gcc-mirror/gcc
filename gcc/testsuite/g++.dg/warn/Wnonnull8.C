/* PR c++/96310 - Ignoring -Wnonnull via pragma gcc diagnostics still produces
   an unwanted note
   { dg-do compile }
   { dg-options "-Wall" } */

struct C {
  void f ();                  // { dg-message "in a call" }
  void g ();                  // { dg-bogus "in a call" }
};

void f ()
{
  static_cast<C*>(0)->f ();   // { dg-warning "\\\[-Wnonnull" }
}

void g ()
{
#pragma GCC diagnostic ignored "-Wnonnull"
  static_cast<C*>(0)->g ();
}
