/* PR c++/101731 */

#pragma acc routine	/* { dg-error "not immediately followed by a single function declaration or definition" "" { target c++ } } */
int foo (int bar ());
