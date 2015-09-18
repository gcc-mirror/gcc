/* { dg-do compile } */
#pragma GCC diagnostic /* { dg-warning "24:missing" "missing" { xfail *-*-* } } */

#pragma GCC diagnostic warn /* { dg-warning "24:expected" } */

#pragma GCC diagnostic ignored "-Wfoo" /* { dg-warning "32:unknown" } */
