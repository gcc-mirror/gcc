/* { dg-do run } */

char one[50] = "ijk";
int
main (void)
{
  return __builtin_strlen (one) != 3;
}
