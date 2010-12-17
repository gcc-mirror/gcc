/* { dg-do compile } */
/* { dg-options "-std=c89 -pedantic -Wno-long-long" } */
/* { dg-message "warnings being treated as errors" "" { target *-*-* } 0 } */

int i = 0LL;

#pragma GCC diagnostic error "-Wlong-long"

int j = 1LL; /* { dg-error "long long" } */
