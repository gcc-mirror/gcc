/* { dg-do compile } */

/* PR tree-optimization/122296 */

typedef unsigned type1;
typedef unsigned __attribute__((vector_size(sizeof(unsigned) ))) type2;
type1 g(type1 a, type1 b)
{
  type1 c = a == b;
  type1 d = (a|b) == 0;
  return c & d;
}

type1 f(type1 a, type1 b)
{
  type1 c = a != b;
  type1 d = (a|b) != 0;
  return c | d;
}
type2 g2(type2 a, type2 b)
{
  type2 c = a == b;
  type2 d = (a|b) == 0;
  return c & d;
}

type2 f2(type2 a, type2 b)
{
  type2 c = a != b;
  type2 d = (a|b) != 0;
  return c | d;
}
