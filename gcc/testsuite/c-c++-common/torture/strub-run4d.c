/* { dg-do run } */
/* { dg-options "-fstrub=strict" } */
/* { dg-require-effective-target alloca } */
/* { dg-require-effective-target strub } */

#define ATTR_STRUB_AT_CALLS __attribute__ ((__strub__ ("at-calls")))

#include "strub-run4.c"
