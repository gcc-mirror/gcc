/* { dg-do compile } */
/* { dg-options "-mcpu=niagara2 -Os" } */

int foo (unsigned long long l)
{
  return __builtin_parityll (l);
}
