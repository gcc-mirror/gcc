/* PR debug/43084 */
/* { dg-do compile } */
/* { dg-options "-O1 -fipa-struct-reorg -fwhole-program -fipa-type-escape -fcompare-debug" } */

struct S
{
  int a;
};

int
main ()
{
  struct S s;
  struct S *p = &s;
  return p->a;
}
