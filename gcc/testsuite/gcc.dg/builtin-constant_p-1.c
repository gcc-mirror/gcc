/* { dg-do compile } */

int main()
{
  if (__builtin_constant_p ()) /* { dg-error "too few arguments" } */
    return 0;
  if (__builtin_constant_p (5, 6)) /* { dg-error "too many arguments" } */
    return 1;
  return 0;
}
