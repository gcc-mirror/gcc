/* { dg-do compile } */
/* { dg-final { scan-assembler "_GLOBAL__I_foobar" } } */
struct foo { foo (); };
foo foobar;
