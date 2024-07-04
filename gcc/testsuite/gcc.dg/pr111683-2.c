/* PR middle-end/111683 */
/* { dg-do run } */
/* { dg-options "-O2" } */

int b[6] = { 3, 4, 5, 6, 7, 8 }, c[12];
int d[16] = { 0, 1, 3, 6, 10, 14, 12, 9, 5, 0, 0, 0 };

int
main ()
{
  int i;
  if (sizeof (int) * 2 != sizeof (long long))
    return 0;
  for (i = 0; i < 6; i++)
    {
      long long a;
      __builtin_memcpy (&a, &c[i], sizeof (a));
      a += (((long long) i) << (sizeof (int) * __CHAR_BIT__)) + i;
      __builtin_memcpy (&c[i], &a, sizeof (a));
      __builtin_memcpy (&a, &c[i + 2], sizeof (a));
      a += (((long long) i) << (sizeof (int) * __CHAR_BIT__)) + i;
      __builtin_memcpy (&c[i + 2], &a, sizeof (a));
    }
  if (__builtin_memcmp (&c[0], &d[0], sizeof (c)))
    __builtin_abort ();
  return 0;
}
