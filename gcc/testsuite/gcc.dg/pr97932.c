/* Verify that we don't emit ranges that span both
   a macro definition location and a macro expansion location.  */

/* { dg-options "-fdiagnostics-show-caret" } */

/* Various cases involving the ranges of the LHS and RHS operands to "-".  */

/* Case 1
   start token is in macro definition ("&"),
   end token is in macro invocation ("a" and "b").  */   

#define M1(A, B) &A - &B /* { dg-error "invalid operands" } */

/* Intervening
   material
   that
   ought
   not
   to
   be
   printed.  */

int test_1 (float a, int b)
{
  return M1(a, b); /* { dg-message "in expansion of macro 'M1'" } */
}

/* { dg-begin-multiline-output "" }
 #define M1(A, B) &A - &B
                     ^
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
   return M1(a, b);
          ^~
   { dg-end-multiline-output "" } */

/* Case 2:
   start and end tokens are both in macro invocation ("&", and "a"/"b").  */   

#define M2(A, B) A - B /* { dg-error "invalid operands" } */

/* Intervening
   material
   that
   ought
   not
   to
   be
   printed.  */

int test_2 (float a, int b)
{
  return M2(&a, &b); /* { dg-message "in expansion of macro 'M2'" } */
}

/* { dg-begin-multiline-output "" }
 #define M2(A, B) A - B
                    ^
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
   return M2(&a, &b);
          ^~
   { dg-end-multiline-output "" } */

/* Case 3:
   start token is in macro invocation ("&"),
   end token is in macro definition ("a").  */   

#define M3(OP) OP a - OP b /* { dg-error "invalid operands" } */

/* Intervening
   material
   that
   ought
   not
   to
   be
   printed.  */

int test_3 (float a, int b)
{
  return M3(&); /* { dg-message "in expansion of macro 'M3'" } */
}

/* { dg-begin-multiline-output "" }
 #define M3(OP) OP a - OP b
                     ^
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
   return M3(&);
          ^~
   { dg-end-multiline-output "" } */


/* Case 4:
   start and end tokens are both in macro definition ("&a").  */   

#define M4 &a - &b /* { dg-error "invalid operands" } */

/* Intervening
   material
   that
   ought
   not
   to
   be
   printed.  */

int test_4 (float a, int b)
{
  return M4; /* { dg-message "in expansion of macro 'M4'" } */
}

/* { dg-begin-multiline-output "" }
 #define M4 &a - &b
            ~~ ^ ~~
            |    |
            |    int *
            float *
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
   return M4;
          ^~
   { dg-end-multiline-output "" } */

