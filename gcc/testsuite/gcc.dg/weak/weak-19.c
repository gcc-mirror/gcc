/* { dg-do compile }
   { dg-require-weak "" } */

/* Verify that the following invalid pragmas are diagnosed.  */

#pragma weak foo = 1   /* { dg-warning "malformed '#pragma weak'" } */
#pragma weak foo = +   /* { dg-warning "malformed '#pragma weak'" } */
#pragma weak foo = ?   /* { dg-warning "malformed '#pragma weak'" } */
