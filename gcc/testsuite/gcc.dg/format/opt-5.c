/* Test diagnostics for options used on their own without
   -Wformat.  -Wformat-zero-length.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wformat-zero-length" } */

/* { dg-warning "warning: -Wformat-zero-length ignored without -Wformat" "ignored" { target *-*-* } 0 } */
