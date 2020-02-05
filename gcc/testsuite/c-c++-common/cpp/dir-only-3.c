/* Copyright 2007 Free Software Foundation, Inc.
   Contributed by Ollie Wild <aaw@google.com>.  */

/* { dg-do preprocess } */
/* { dg-options "-fdirectives-only -H" } */
/* { dg-message "dir-only-3a\.h\n\[^\n\]*dir-only-3b\.h\n\[^\n\]*dir-only-3a\.h\n" "include guard check" { target *-*-* } 0 } */

/* Tests include guards. */

#include "dir-only-3a.h"
#include "dir-only-3b.h"
#include "dir-only-3b.h"
#include "dir-only-3a.h"
