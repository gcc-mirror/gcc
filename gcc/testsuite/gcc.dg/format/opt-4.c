/* Test diagnostics for options used on their own without
   -Wformat.  -Wformat-y2k.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wformat-y2k" } */

/* { dg-warning "'-Wformat-y2k' ignored without '-Wformat'" "ignored" { target *-*-* } 0 } */
