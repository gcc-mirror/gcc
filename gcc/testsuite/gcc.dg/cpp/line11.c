/* PR c/99325 */
/* { dg-do preprocess } */
/* { dg-options "-pedantic" } */

#line 4294967295	/* { dg-warning "line number out of range" } */
#pragma message "foo"
