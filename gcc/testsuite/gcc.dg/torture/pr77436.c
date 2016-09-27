/* { dg-do run } */

int main()
{
  unsigned short sum = 0;
  for (short x = -(__SHRT_MAX__ -1); x <= (__SHRT_MAX__ -1); x++)
    sum += x;
  if (sum != 0)
    __builtin_abort ();
  return 0;
}
