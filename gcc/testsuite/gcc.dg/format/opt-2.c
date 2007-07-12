/* Test diagnostics for options used on their own without
   -Wformat.  -Wformat-nonliteral.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wformat-nonliteral" } */

/* { dg-warning "-Wformat-nonliteral ignored without -Wformat" "ignored" { target *-*-* } 0 } */
