/* { dg-do compile } */
/* { dg-options "-fstrub=strict" } */
/* { dg-require-effective-target strub } */

/* It would be desirable to issue at least warnings for these.  */

typedef int __attribute__ ((__strub__)) strub_int;
strub_int *ptr;

int *f () {
  return ptr; /* { dg-message "incompatible|invalid conversion" } */
}

strub_int *g () {
  return f (); /* { dg-message "incompatible|invalid conversion" } */
}
