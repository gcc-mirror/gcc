/* Test diagnostics for options used on their own without
   -Wformat.  -Wmissing-format-attribute.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wmissing-format-attribute" } */

/* { dg-warning "warning: -Wmissing-format-attribute ignored without -Wformat" "ignored" { target *-*-* } 0 } */
