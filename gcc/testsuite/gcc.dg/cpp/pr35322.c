/* Test case for PR 35322 -- _Pragma ICE.  */

/* { dg-do preprocess } */
_Pragma("GCC dependency") /* { dg-error "'#pragma GCC dependency' expects" } */
