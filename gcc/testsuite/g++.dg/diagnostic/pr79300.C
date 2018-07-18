// { dg-options "-fdiagnostics-show-caret" }

#define TEST_1_DEPTH_0 throw bad_alloc; // { dg-line define_TEST_1_DEPTH_0 }

void test_1 ()
{
  TEST_1_DEPTH_0 // { dg-line use_TEST_1_DEPTH_0 }
}

// { dg-error "'bad_alloc' was not declared in this scope" "" { target *-*-* } define_TEST_1_DEPTH_0 }
/* { dg-begin-multiline-output "" }
 #define TEST_1_DEPTH_0 throw bad_alloc;
                              ^~~~~~~~~
   { dg-end-multiline-output "" } */
// { dg-message "in expansion of macro 'TEST_1_DEPTH_0'" "" { target *-*-* } use_TEST_1_DEPTH_0 }
/* { dg-begin-multiline-output "" }
   TEST_1_DEPTH_0
   ^~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */


#define TEST_2_DEPTH_0 throw bad_alloc; // { dg-line define_TEST_2_DEPTH_0 }
#define TEST_2_DEPTH_1 TEST_2_DEPTH_0 // { dg-line define_TEST_2_DEPTH_1 }

void test_2 ()
{
  TEST_2_DEPTH_1 // { dg-line use_TEST_2_DEPTH_1 }
}

// { dg-error "'bad_alloc' was not declared in this scope" "" { target *-*-* } define_TEST_2_DEPTH_0 }
/* { dg-begin-multiline-output "" }
 #define TEST_2_DEPTH_0 throw bad_alloc;
                              ^~~~~~~~~
   { dg-end-multiline-output "" } */
// { dg-message "in expansion of macro 'TEST_2_DEPTH_0'" "" { target *-*-* } define_TEST_2_DEPTH_1 }
/* { dg-begin-multiline-output "" }
 #define TEST_2_DEPTH_1 TEST_2_DEPTH_0
                        ^~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
// { dg-message "in expansion of macro 'TEST_2_DEPTH_1'" "" { target *-*-* } use_TEST_2_DEPTH_1 }
/* { dg-begin-multiline-output "" }
   TEST_2_DEPTH_1
   ^~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
