/* PR preprocessor/80753 */
/* { dg-do compile } */
/* { dg-options "" } */

#if __has_include("nonexistent.h")
# error
#endif

#include "nonexistent.h"

/* { dg-message "nonexistent.h" "nonexistent.h" { target *-*-* } 0 } */
/* { dg-message "terminated" "terminated" { target *-*-* } 0 } */

/* This declaration should not receive any diagnostic.  */
foo bar;
