// { dg-options "-fdiagnostics-show-caret" }

enum { LASAGNA, SPAGHETTI };
namespace outer_ns_a
{
  enum enum_in_outer_ns_a { STRAWBERRY, BANANA };
  namespace inner_ns
  {
    enum enum_in_inner_ns { ELEPHANT, LION };
  }
}
namespace outer_ns_b
{
  enum enum_in_outer_ns_b { NIGHT, DAY };
}

void misspelled_enum_in_global_ns ()
{
  SPOOGHETTI; // { dg-error "'SPOOGHETTI' was not declared in this scope; did you mean 'SPAGHETTI'" }
  /* { dg-begin-multiline-output "" }
   SPOOGHETTI;
   ^~~~~~~~~~
   SPAGHETTI
     { dg-end-multiline-output "" } */
}

void unqualified_enum_in_outer_ns ()
{
  BANANA; // { dg-error "'BANANA' was not declared in this scope; did you mean 'outer_ns_a::BANANA'" }
  /* { dg-begin-multiline-output "" }
   BANANA;
   ^~~~~~
   outer_ns_a::BANANA
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
   enum enum_in_outer_ns_a { STRAWBERRY, BANANA };
                                         ^~~~~~
     { dg-end-multiline-output "" } */
}

namespace outer_ns_a
{
  void misspelled_unqualified_enum_in_outer_ns () {
    BANANAS; // { dg-error "'BANANAS' was not declared in this scope; did you mean 'BANANA'" }
  /* { dg-begin-multiline-output "" }
     BANANAS;
     ^~~~~~~
     BANANA
     { dg-end-multiline-output "" } */
  }
};

void unqualified_enum_in_inner_ns ()
{
  ELEPHANT; // { dg-error "'ELEPHANT' was not declared in this scope; did you mean 'outer_ns_a::inner_ns::ELEPHANT'" }
  /* { dg-begin-multiline-output "" }
   ELEPHANT;
   ^~~~~~~~
   outer_ns_a::inner_ns::ELEPHANT
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
     enum enum_in_inner_ns { ELEPHANT, LION };
                             ^~~~~~~~
     { dg-end-multiline-output "" } */
}

void partially_qualified_enum_in_inner_ns ()
{
  outer_ns_a::ELEPHANT; // { dg-error "'ELEPHANT' is not a member of 'outer_ns_a'; did you mean 'outer_ns_a::inner_ns::ELEPHANT'" }
  /* { dg-begin-multiline-output "" }
   outer_ns_a::ELEPHANT;
               ^~~~~~~~
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
     enum enum_in_inner_ns { ELEPHANT, LION };
                             ^~~~~~~~
     { dg-end-multiline-output "" } */
}

void wrongly_qualified_enum ()
{
  outer_ns_a::NIGHT; // { dg-error "'NIGHT' is not a member of 'outer_ns_a'; did you mean 'outer_ns_b::NIGHT'" }
  /* { dg-begin-multiline-output "" }
   outer_ns_a::NIGHT;
               ^~~~~
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
   enum enum_in_outer_ns_b { NIGHT, DAY };
                             ^~~~~
     { dg-end-multiline-output "" } */
}
