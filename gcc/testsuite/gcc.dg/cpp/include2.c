/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Tests that #include does not allow the terminating '>' or '"' to be
   escaped, as per the standard.  */

/* Source: Neil Booth, 4 Nov 2000.  */

#include <silly\>>  /* { dg-warning "extra tokens" "" } */
#include "silly\""  /* { dg-error "missing" "" } */

/* These first 2 errors are No such file or directory.  However, this
   message is locale-dependent, so don't test for it.  */
/* { dg-error "silly" "" { target *-*-* } 10 } */
/* { dg-error "silly" "" { target *-*-* } 11 } */
/* { dg-warning "extra tokens" "" { target *-*-* } 11 } */
