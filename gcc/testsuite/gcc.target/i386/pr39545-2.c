/* PR target/39545 */
/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2" } */

struct flex
{
  int i;
  int flex [];
};

struct flex
foo (int x)
{ /* { dg-message "note: the ABI of passing struct with a flexible array member has changed in GCC 4.4" } */
  struct flex s;
  s.i = x;
  return s;
}
