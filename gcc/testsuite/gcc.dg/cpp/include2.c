/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Tests that #include does not allow the terminating '>' or '"' to be
   escaped, as per the standard.  */

/* Source: Neil Booth, 4 Nov 2000.  */

#include <silly\>>  /* { dg-warning "extra tokens" "" } */
#include "silly\""  /* { dg-error "missing" "" } */

/* { dg-error "No such file" "" { target *-*-* } 10 } */
/* { dg-error "No such file" "" { target *-*-* } 11 } */
/* { dg-warning "extra tokens" "" { target *-*-* } 11 } */
