/* { dg-do run } */
/* { dg-additional-options "-fstrict-overflow" } */

int main()
{
  signed char var_0, var_1 = -128;
  var_0 = (signed char)(-var_1) / 3;
  if (var_0 > 0)
    __builtin_abort();
}
