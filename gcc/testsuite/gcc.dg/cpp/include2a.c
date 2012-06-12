/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Tests that #include does not allow the terminating '>' or '"' to be
   escaped, as per the standard.  */

/* Source: Neil Booth, 4 Nov 2000.  */

#include "silly\""  /* { dg-error "extra tokens" "" } */

/* These error is No such file or directory, just once.  However, this
   message is locale-dependent, so don't test for it.  */
/* { dg-error "silly" "silly" { target *-*-* } 0 } */
/* { dg-error "missing" "missing" { target *-*-* } 0 } */
/* { dg-message "terminated" "terminated" { target *-*-* } 0 } */
