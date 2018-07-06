/* PR preprocessor/69869 */
/* { dg-do preprocess } */
/* { dg-options "-traditional-cpp" } */

#define C(a,b)a/**/b
C (foo/,**/)
C (foo/,*)
/* { dg-error "-:unterminated comment" "" {target "*-*-*"} .-1 } */
