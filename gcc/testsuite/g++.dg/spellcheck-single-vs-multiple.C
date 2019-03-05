/* Example of namespace suggestions, covering the special-case handling
   of where there's one suggestion, vs multiple suggestions.  */

/* { dg-options "-fdiagnostics-show-caret" } */

/* Missing a namespace, where there's one candidate.
   Verify that we issue a fix-it hint.  */

namespace ns1
{
  void foo_1 (); // { dg-line foo_1_decl }
}

void test_1 ()
{
  foo_1 (); // { dg-error "'foo_1' was not declared in this scope; did you mean 'ns1::foo_1'\\?" }
  /* { dg-begin-multiline-output "" }
   foo_1 ();
   ^~~~~
   ns1::foo_1
     { dg-end-multiline-output "" } */
  // { dg-message "'ns1::foo_1' declared here" "" { target *-*-*} foo_1_decl }
  /* { dg-begin-multiline-output "" }
   void foo_1 ();
        ^~~~~
     { dg-end-multiline-output "" } */
}

/* Missing a namespace, where there are multiple candidates.
   We don't issue a fix-it hint.  */

namespace ns2_a
{
  char foo_2 (); // { dg-line ns2_a_foo_2_decl }
}

namespace ns2_b
{
  int foo_2 (); // { dg-line ns2_b_foo_2_decl }
}

void test_2 ()
{
  foo_2 (); // { dg-line foo_2_usage }
  // { dg-error "'foo_2' was not declared in this scope" "" { target *-*-*} foo_2_usage }
  /* { dg-begin-multiline-output "" }
   foo_2 ();
   ^~~~~
     { dg-end-multiline-output "" } */
  // { dg-message "suggested alternatives:" "" { target *-*-*} foo_2_usage }
  // { dg-message "  'ns2_a::foo_2'" "" { target *-*-*} ns2_a_foo_2_decl }
  /* { dg-begin-multiline-output "" }
   char foo_2 ();
        ^~~~~
     { dg-end-multiline-output "" } */
  // { dg-message "  'ns2_b::foo_2'" "" { target *-*-*} ns2_b_foo_2_decl }
  /* { dg-begin-multiline-output "" }
   int foo_2 ();
       ^~~~~
     { dg-end-multiline-output "" } */
}

/* Misspelling within an explicit namespace.
   Verify that we issue a fix-it hint.  */

namespace ns3
{
  void foo_3 ();
}

void test_3 ()
{
  ns3::goo_3 (); // { dg-error "'goo_3' is not a member of 'ns3'; did you mean 'foo_3'\\?" }
  /* { dg-begin-multiline-output "" }
   ns3::goo_3 ();
        ^~~~~
        foo_3
     { dg-end-multiline-output "" } */
}
