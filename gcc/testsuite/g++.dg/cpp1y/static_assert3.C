// { dg-do compile { target c++11 } }
// { dg-options "-fdiagnostics-show-caret" }

class is_not_empty
{
  int i;
};

/* Verify location of static_assert failure (and of traits).  */

static_assert(__is_empty(is_not_empty), "message"); // { dg-error "static assertion failed: message" }
/* { dg-begin-multiline-output "" }
 static_assert(__is_empty(is_not_empty), "message");
               ^~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */


/* Again, this time verifying location of "noexcept".  */

extern void might_throw ();

static_assert(noexcept(might_throw ()), "message"); // { dg-error "static assertion failed: message" }
/* { dg-begin-multiline-output "" }
 static_assert(noexcept(might_throw ()), "message");
               ^~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
