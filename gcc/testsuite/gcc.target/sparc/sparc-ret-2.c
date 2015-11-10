/* PR target/57845 */

/* { dg-do compile } */
/* { dg-options "-freg-struct-return" } */

struct S { short int i; };

struct S foo (short int i)
{
  struct S s;
  s.i = i;
  return s;
}
