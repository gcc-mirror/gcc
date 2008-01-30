/* { dg-do compile } */
/* { dg-options "-fpie" { target { ! nonpic } } } */
/* { dg-final { scan-assembler "_GLOBAL__I(_|_65535_0_)foobar" } } */

struct foo { foo (); };
foo foobar;
