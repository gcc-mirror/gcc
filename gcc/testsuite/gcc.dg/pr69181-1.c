/* { dg-do compile { target this_will_not_be_matched-*-* } } */

/* { dg-begin-multiline-output "" }
   This message should never be checked for.
   In particular, it shouldn't be checked for in the *next*
   test case.
   { dg-end-multiline-output "" } */
