/* Test that the null directive doesn't swallow the following line.  */

/* { dg-do preprocess } */

#
#error OK	/* { dg-error "OK" } */
