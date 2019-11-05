/* { dg-do preprocess } */
/* { dg-options "-std=c11" { target c } } */

#define A(x, y) x##y
A(<=, >)	/* { dg-error "does not give a valid preprocessing token" "" { target { ! c++2a } } } */
A(<=>, >)	/* { dg-error "does not give a valid preprocessing token" "" { target c++2a } } */
