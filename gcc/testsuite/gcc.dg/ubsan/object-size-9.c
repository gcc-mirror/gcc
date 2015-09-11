/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=undefined" } */

struct T { int c; char d[]; };
struct T t = { 1, "a" };

int
baz (int i)
{
  return t.d[i];
}

int
main (void)
{
  baz (2);
  return 0;
}

/* { dg-output "load of address \[^\n\r]* with insufficient space for an object of type 'char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^" } */
