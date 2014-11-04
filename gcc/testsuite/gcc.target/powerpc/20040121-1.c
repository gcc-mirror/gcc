/* { dg-do compile { target powerpc*-*-darwin* } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=G5" } } */
/* { dg-options "-O2 -mcpu=G5 " } */

long long (*y)(int t);
long long get_alias_set (int t)
{
  return y(t);
}
