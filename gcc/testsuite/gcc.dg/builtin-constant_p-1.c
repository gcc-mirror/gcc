/* { dg-do compile } */

int main()
{
  if (__builtin_constant_p ()) /* { dg-error "not enough" } */
    return 0;
  if (__builtin_constant_p (5, 6)) /* { dg-error "too many" } */
    return 1;
  return 0;
}
