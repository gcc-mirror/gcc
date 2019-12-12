/* Test diagnostics for options used on their own without
   -Wformat.  -Wformat-extra-args.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wformat-extra-args" } */

/* { dg-warning "'-Wformat-extra-args' ignored without '-Wformat'" "ignored" { target *-*-* } 0 } */
