/* { dg-do run } */

int main()
{
  _Bool b;
  *(char *)&b = 123;
  if (*(char *)&b != 123)
    __builtin_abort ();
  return 0;
}
