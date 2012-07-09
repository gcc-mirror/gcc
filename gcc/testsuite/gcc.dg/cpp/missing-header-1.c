/* Test that missing headers are fatal errors.  PR 15638.  */
/* { dg-do compile } */
/* { dg-options "" } */

#include "nonexistent.h"
/* { dg-message "nonexistent.h" "nonexistent.h" { target *-*-* } 0 } */
/* { dg-message "terminated" "terminated" { target *-*-* } 0 } */

/* This declaration should not receive any diagnostic.  */
foo bar;
