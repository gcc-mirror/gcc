/* Test diagnostics for options used on their own without
   -Wformat.  -Wformat-security.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wformat-security" } */

/* { dg-warning "'-Wformat-security' ignored without '-Wformat'" "ignored" { target *-*-* } 0 } */
