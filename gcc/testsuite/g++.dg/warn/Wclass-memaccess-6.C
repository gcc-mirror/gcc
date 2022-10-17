/* PR c++/98835 - -Wclass-memaccess with class with ref-qualified
   copy-assignment operator
   { dg-do compile { target { c++11 } } }
   { dg-options "-Wall" } */

struct Bad
{
  Bad* operator& () { return this; }
  Bad & operator=(Bad const &) & = default;
};

void test ()
{
  static_assert (__has_trivial_copy (Bad));

  // T () = T ();                                      // error
  __builtin_memcpy (&Bad (), &Bad (), sizeof (Bad));   // { dg-warning "\\\[-Wclass-memaccess" }
}
