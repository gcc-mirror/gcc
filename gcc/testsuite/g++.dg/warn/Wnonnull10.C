/* Very that -Wnonnull is issued for calls to inline member functions
   with a null this pointer.
   { dg-do compile }
   { dg-options "-Wall" } */

#if __cplusplus < 201103L
# define nullptr 0
#endif

struct S
{
  void f () { }
  static void g () { }
  virtual void h () { }
};

void f0 ()
{
  static_cast<S*>(0)->f ();         // { dg-warning "-Wnonnull" }
  static_cast<S*>(0)->g ();
  static_cast<S*>(0)->h ();         // { dg-warning "-Wnonnull" }
}

void f1 ()
{
  static_cast<S*>(nullptr)->f ();   // { dg-warning "-Wnonnull" }
  static_cast<S*>(nullptr)->g ();
  static_cast<S*>(nullptr)->h ();   // { dg-warning "-Wnonnull" }
}

void f2 ()
{
  S* const p = 0;

  p->f ();                          // { dg-warning "-Wnonnull" }
  p->g ();
  p->h ();                          // { dg-warning "-Wnonnull" }
}


#pragma GCC optimize "1"

void f3 ()
{
  S *p = 0;

  p->f ();                          // { dg-warning "-Wnonnull" }
  p->g ();
  p->h ();                          // { dg-warning "-Wnonnull" }
}


#pragma GCC optimize "2"

void f4 (S *p)
{
  if (p)
    return;

  p->f ();                          // { dg-warning "-Wnonnull" }
  p->g ();
  p->h ();                          // { dg-warning "-Wnonnull" }
}
