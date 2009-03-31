/* Test that missing headers are fatal errors.  PR 15638.  */
/* { dg-do compile } */
/* { dg-options "" } */

#include "nonexistent.h" /* { dg-error "nonexistent.h" } */
/* { dg-message "terminated" "" { target *-*-* } 0 } */

/* This declaration should not receive any diagnostic.  */
foo bar;
