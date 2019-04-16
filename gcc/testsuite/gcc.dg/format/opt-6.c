/* Test diagnostics for options used on their own without
   -Wformat.  -Wformat-contains-nul.  */
/* Origin: Bruce Korb <bkorb@gnu.org> */
/* { dg-do compile } */
/* { dg-options "-Wformat-contains-nul" } */

/* { dg-warning "'-Wformat-contains-nul' ignored without '-Wformat'" "ignored" { target *-*-* } 0 } */
