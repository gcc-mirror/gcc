/* PR preprocessor/97471 */
/* { dg-do compile } */

/* ICE with non-fn use of fn-like macro at EOF  */

#define a() b

/* { dg-error "expected" "" { target c } .+2 } */
/* { dg-error "does not name" "" { target c++ } .+1 } */
a
