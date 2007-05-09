/* { dg-do compile } */
/* { dg-options "-fpie" { target *-*-darwin* } } */
/* { dg-final { scan-assembler "_GLOBAL__I_foobar" } } */

struct foo { foo (); };
foo foobar;
