/* Test for proper suppression of warnings in system headers,
   and only in system headers.  */
/* FRAGILITY WARNING: The only way we have to distinguish the good error
   from the bad error is that the good error is on line 4 and the bad is
   on line 5 (of their respective files).  dg.exp doesn't have any way to
   condition error matchers on the file they're in.  */

/* { dg-do preprocess } */
/* { dg-error "include_next" "good error" { target *-*-* } 4 } */

#include "syshdr1.h"  /* { dg-error "" "In file included from:" } */
#include "syshdr2.h"
