/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

signed char a = 0, b = 0, c = 0, d = 0;

int main()
{
  a = -(b - 405418259) - ((d && c) ^ 2040097152);
  if (a != (signed char) -1634678893)
    __builtin_abort ();
  return 0;
}
