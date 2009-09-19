/* Test that missing system headers are fatal errors with -MMD.  */
/* { dg-do compile } */
/* { dg-options "-MMD" } */

#include <nonexistent.h> /* { dg-error "nonexistent.h" } */
/* { dg-message "terminated" "" { target *-*-* } 0 } */
