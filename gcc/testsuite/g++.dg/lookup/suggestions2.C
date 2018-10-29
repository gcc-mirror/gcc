/* Suggestions involving namespaces.

   The long variable names in this test case are close enough that we offer
   spellchecking suggestions for them in the given namespace, with fix-it
   hints.

   The short variable names don't get spellchecking suggestions; instead
   we offer suggestions about other namespaces.  However, as we don't
   reliably have location information about the namespace part of the name,
   we shouldn't offer fix-it hints for such cases.  */

// { dg-do compile }
// { dg-options "-fdiagnostics-show-caret" }

namespace outer_ns {
  int var_in_outer_ns; // { dg-line decl_of_var_in_outer_ns }
  int o; // { dg-line decl_of_o }

  namespace inner_ns_a {
    int var_in_inner_ns_a;
    int a; // { dg-line decl_of_a }
  }
  namespace inner_ns_b {
    int var_in_inner_ns_b;
    int b; // { dg-line decl_of_b }
  }
}

/* This one should get spell-corrected within the same namespace,
   with a fix-it hint.  */

int test_1_long (void) {
  return outer_ns::var_in_inner_ns_a; // { dg-error "did you mean 'var_in_outer_ns'" }
  /* { dg-begin-multiline-output "" }
   return outer_ns::var_in_inner_ns_a;
                    ^~~~~~~~~~~~~~~~~
                    var_in_outer_ns
     { dg-end-multiline-output "" } */
}

/* This one should get a namespace suggestion (child namespace),
   with no fix-it hint.  */

int test_1_short (void) {
  return outer_ns::a; // { dg-error "did you mean 'outer_ns::inner_ns_a::a'" }
  /* { dg-begin-multiline-output "" }
   return outer_ns::a;
                    ^
     { dg-end-multiline-output "" } */
  // { dg-message "declared here" "" { target *-*-*} decl_of_a }
  /* { dg-begin-multiline-output "" }
     int a;
         ^
     { dg-end-multiline-output "" } */
}

/* This one should get spell-corrected within the same namespace,
   with a fix-it hint.  */

int test_2_long (void) {
  return outer_ns::inner_ns_a::var_in_outer_ns; // { dg-error "did you mean 'var_in_inner_ns_a'" }
  /* { dg-begin-multiline-output "" }
   return outer_ns::inner_ns_a::var_in_outer_ns;
                                ^~~~~~~~~~~~~~~
                                var_in_inner_ns_a
     { dg-end-multiline-output "" } */
}

/* This one should get a namespace suggestion (parent namespace),
   with no fix-it hint.  */

int test_2_short (void) {
  return outer_ns::inner_ns_a::o; // { dg-error "did you mean 'outer_ns::o'" }
  /* { dg-begin-multiline-output "" }
   return outer_ns::inner_ns_a::o;
                                ^
     { dg-end-multiline-output "" } */
  // { dg-message "declared here" "" { target *-*-*} decl_of_o }
  /* { dg-begin-multiline-output "" }
   int o;
       ^
     { dg-end-multiline-output "" } */
}

/* This one should get spell-corrected within the same namespace,
   with a fix-it hint.  */

int test_3_long (void) {
  return outer_ns::inner_ns_a::var_in_inner_ns_b; // { dg-error "did you mean 'var_in_inner_ns_a'" }
  /* { dg-begin-multiline-output "" }
   return outer_ns::inner_ns_a::var_in_inner_ns_b;
                                ^~~~~~~~~~~~~~~~~
                                var_in_inner_ns_a
     { dg-end-multiline-output "" } */
}

/* This one should get a namespace suggestion (sibling namespace),
   with no fix-it hint.  */

int test_3_short (void) {
  return outer_ns::inner_ns_a::b; // { dg-error "did you mean 'outer_ns::inner_ns_b::b'" }
  /* { dg-begin-multiline-output "" }
   return outer_ns::inner_ns_a::b;
                                ^
     { dg-end-multiline-output "" } */
  // { dg-message "declared here" "" { target *-*-*} decl_of_b }
  /* { dg-begin-multiline-output "" }
     int b;
         ^
     { dg-end-multiline-output "" } */
}

/* This one should get a namespace suggestion, from the global ns to a child ns.
   It should get a fix-it hint.  */

int test_4_long (void) {
  return ::var_in_outer_ns; // { dg-error "did you mean 'outer_ns::var_in_outer_ns'" }
  /* { dg-begin-multiline-output "" }
   return ::var_in_outer_ns;
            ^~~~~~~~~~~~~~~
            outer_ns::var_in_outer_ns
     { dg-end-multiline-output "" } */
  // { dg-message "declared here" "" { target *-*-*} decl_of_var_in_outer_ns }
  /* { dg-begin-multiline-output "" }
   int var_in_outer_ns;
       ^~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */
}
