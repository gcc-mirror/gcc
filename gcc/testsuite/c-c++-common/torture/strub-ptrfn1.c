/* { dg-do compile } */
/* { dg-options "-fstrub=strict" } */
/* { dg-require-effective-target strub } */

typedef void ft (void);
typedef void ft2 (int, int);
extern ft __attribute__ ((__strub__)) fnac;

ft * f (void) {
  return fnac; /* { dg-message "incompatible|invalid conversion" } */
}
