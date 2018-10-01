/* PR gcov-profile/86957 */
/* { dg-do compile } */
/* { dg-options "-fprofile-use" } */

void foo () { } /* { dg-warning "profile count data file not found" } */
