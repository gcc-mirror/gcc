/* Test that missing system headers are fatal errors with -MD.  */
/* { dg-do compile } */
/* { dg-options "-MD" } */

#include <nonexistent.h> /* { dg-error "nonexistent.h" } */
/* { dg-message "terminated" "" { target *-*-* } 0 } */
