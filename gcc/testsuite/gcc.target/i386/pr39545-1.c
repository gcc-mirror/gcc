/* PR target/39545 */
/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2" } */

struct flex
{
  int i;
  int flex [];
};

int
foo (struct flex s) /* { dg-message "note: The ABI of passing struct with a flexible array member has changed in GCC 4.4" } */
{
  return s.i;
}

struct flex
bar (int x)
{
  struct flex s;
  s.i = x;
  return s;
}
