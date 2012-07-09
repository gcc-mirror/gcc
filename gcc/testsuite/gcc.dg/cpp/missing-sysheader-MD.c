/* Test that missing system headers are fatal errors with -MD.  PR 28435.  */
/* { dg-do compile } */
/* { dg-options "-MD" } */

#include <nonexistent.h>
/* { dg-message "nonexistent.h" "nonexistent.h" { target *-*-* } 0 } */
/* { dg-message "terminated" "terminated" { target *-*-* } 0 } */

/* This declaration should not receive any diagnostic.  */
foo bar;
