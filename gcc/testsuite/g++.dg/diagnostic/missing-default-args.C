// { dg-options "-fdiagnostics-show-caret" }

/* Function.  */

void test_1 (int a, int b = 42, int c, int d); // { dg-line test_1 }

// { dg-error "default argument missing for parameter 3 of " "" { target *-*-* } test_1 }
/* { dg-begin-multiline-output "" }
 void test_1 (int a, int b = 42, int c, int d);
                                 ~~~~^
   { dg-end-multiline-output "" } */
// { dg-message "...following parameter 2 which has a default argument" "" { target *-*-* } test_1 }
/* { dg-begin-multiline-output "" }
 void test_1 (int a, int b = 42, int c, int d);
                     ~~~~^~~~~~
   { dg-end-multiline-output "" } */
// { dg-error "default argument missing for parameter 4 of " "" { target *-*-* } test_1 }
/* { dg-begin-multiline-output "" }
 void test_1 (int a, int b = 42, int c, int d);
                                        ~~~~^
   { dg-end-multiline-output "" } */


/* Non-static member fn.  */

struct test_2
{
  void member_2 (int a, int b = 42, int c); // { dg-line test_2 }
};
// { dg-error "default argument missing for parameter 3 of " "" { target *-*-* } test_2 }
/* { dg-begin-multiline-output "" }
   void member_2 (int a, int b = 42, int c);
                                     ~~~~^
   { dg-end-multiline-output "" } */
// { dg-message "...following parameter 2 which has a default argument" "" { target *-*-* } test_2 }
/* { dg-begin-multiline-output "" }
   void member_2 (int a, int b = 42, int c);
                         ~~~~^~~~~~
   { dg-end-multiline-output "" } */


/* Static member fn.  */

struct test_3
{
  static void member_3 (int a, int b = 42, int c); // { dg-line test_3 }
};
// { dg-error "default argument missing for parameter 3 of " "" { target *-*-* } test_3 }
/* { dg-begin-multiline-output "" }
   static void member_3 (int a, int b = 42, int c);
                                            ~~~~^
   { dg-end-multiline-output "" } */
// { dg-message "...following parameter 2 which has a default argument" "" { target *-*-* } test_3 }
/* { dg-begin-multiline-output "" }
   static void member_3 (int a, int b = 42, int c);
                                ~~~~^~~~~~
   { dg-end-multiline-output "" } */


/* Template.  */

template <typename Type>
void test_4 (int a, int b = 42, int c); // { dg-line test_4 }
// { dg-error "default argument missing for parameter 3 of " "" { target *-*-* } test_4 }
/* { dg-begin-multiline-output "" }
 void test_4 (int a, int b = 42, int c);
                                 ~~~~^
   { dg-end-multiline-output "" } */
// { dg-message "...following parameter 2 which has a default argument" "" { target *-*-* } test_4 }
/* { dg-begin-multiline-output "" }
 void test_4 (int a, int b = 42, int c);
                     ~~~~^~~~~~
   { dg-end-multiline-output "" } */
