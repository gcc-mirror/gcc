// { dg-do compile { target c++11 } }
// { dg-options "-fdiagnostics-show-caret" }

/* Verify that we offer suggestions for misspelled values
   in scoped enums, and for values from scoped enums that
   are missing their scope.
   Verify that we emit fix-it hints for those cases for
   which we have adequate location information.  */

enum class vegetable { CARROT, TURNIP }; // { dg-line decl_of_vegetable }
namespace pasta
{
  enum class shape { LASAGNA, LINGUINE, SPAGHETTI, TAGLIATELLE }; // { dg-line decl_of_shape }
}

void misspelled_value_in_scoped_enum ()
{
  vegetable::TURNUP; // { dg-error "'TURNUP' is not a member of 'vegetable'; did you mean 'TURNIP'" }
  /* { dg-begin-multiline-output "" }
   vegetable::TURNUP;
              ^~~~~~
              TURNIP
     { dg-end-multiline-output "" } */
}

void misspelled_value_using_explicit_ns ()
{
  pasta::shape::SPOOGHETTI; // { dg-error "'SPOOGHETTI' is not a member of 'pasta::shape'; did you mean 'SPAGHETTI'" }
  /* { dg-begin-multiline-output "" }
   pasta::shape::SPOOGHETTI;
                 ^~~~~~~~~~
                 SPAGHETTI
     { dg-end-multiline-output "" } */
}

namespace pasta {
void misspelled_value_using_implicit_ns ()
{
  shape::SPOOGHETTI; // { dg-error "'SPOOGHETTI' is not a member of 'pasta::shape'; did you mean 'SPAGHETTI'" }
  /* { dg-begin-multiline-output "" }
   shape::SPOOGHETTI;
          ^~~~~~~~~~
          SPAGHETTI
     { dg-end-multiline-output "" } */
}
} // namespace pasta

void unqualified_enum_in_global_ns ()
{
  CARROT; // { dg-error "'CARROT' was not declared in this scope; did you mean 'vegetable::CARROT'" }
  /* { dg-begin-multiline-output "" }
   CARROT;
   ^~~~~~
   vegetable::CARROT
     { dg-end-multiline-output "" } */
  // { dg-message "'vegetable::CARROT' declared here" "" { target *-*-* } decl_of_vegetable }
  /* { dg-begin-multiline-output "" }
 enum class vegetable { CARROT, TURNIP };
                        ^~~~~~
     { dg-end-multiline-output "" } */
}

void unqualified_enum_in_ns ()
{
  LASAGNA; // { dg-error "'LASAGNA' was not declared in this scope; did you mean 'pasta::shape::LASAGNA'" }
  /* { dg-begin-multiline-output "" }
   LASAGNA;
   ^~~~~~~
   pasta::shape::LASAGNA
     { dg-end-multiline-output "" } */
  // { dg-message "'pasta::shape::LASAGNA' declared here" "" { target *-*-* } decl_of_shape }
  /* { dg-begin-multiline-output "" }
   enum class shape { LASAGNA, LINGUINE, SPAGHETTI, TAGLIATELLE };
                      ^~~~~~~
     { dg-end-multiline-output "" } */
}

/* We can't guarantee location information here, so don't expect a
   fix-it hint.  */

void unqualified_enum_in_explicit_ns ()
{
  pasta::LINGUINE; // { dg-error "'LINGUINE' is not a member of 'pasta'; did you mean 'pasta::shape::LINGUINE'" }
  /* { dg-begin-multiline-output "" }
   pasta::LINGUINE;
          ^~~~~~~~
     { dg-end-multiline-output "" } */
  // { dg-message "'pasta::shape::LINGUINE' declared here" "" { target *-*-* } decl_of_shape }
  /* { dg-begin-multiline-output "" }
   enum class shape { LASAGNA, LINGUINE, SPAGHETTI, TAGLIATELLE };
                               ^~~~~~~~
     { dg-end-multiline-output "" } */
}

namespace pasta {
void unqualified_enum_in_implicit_ns ()
{
  TAGLIATELLE; // { dg-error "'TAGLIATELLE' was not declared in this scope; did you mean 'pasta::shape::TAGLIATELLE'" }
  /* { dg-begin-multiline-output "" }
   TAGLIATELLE;
   ^~~~~~~~~~~
   pasta::shape::TAGLIATELLE
     { dg-end-multiline-output "" } */
  // { dg-message "'pasta::shape::TAGLIATELLE' declared here" "" { target *-*-* } decl_of_shape }
  /* { dg-begin-multiline-output "" }
   enum class shape { LASAGNA, LINGUINE, SPAGHETTI, TAGLIATELLE };
                                                    ^~~~~~~~~~~
     { dg-end-multiline-output "" } */
}
}
