/* Test for various cases of excess initializers for char arrays,
   bug 107926. */
/* { dg-do compile } */
/* { dg-options "" } */


char s0[] = {"abc",1}; /* { dg-error "'char..' initializer|near init" } */
char s1[] = {"abc","a"}; /* { dg-error "'char..' initializer|near init" } */
char s2[] = {1,"abc"}; /* { dg-error "'char..' initializer|near init|computable at load time" } */
/* { dg-error "integer from pointer without a cast" "" { target *-*-* } .-1 } */

char s3[5] = {"abc",1}; /* { dg-error "'char.5.' initializer|near init" } */
char s4[5] = {"abc","a"}; /* { dg-error "'char.5.' initializer|near init" } */
char s5[5] = {1,"abc"}; /* { dg-error "'char.5.' initializer|near init|computable at load time" } */
/* { dg-error "integer from pointer without a cast" "" { target *-*-* } .-1 } */
