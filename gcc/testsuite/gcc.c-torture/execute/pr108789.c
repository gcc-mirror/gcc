/* PR middle-end/108789 */

int
add (unsigned *r, const unsigned *a, const unsigned *b)
{
  return __builtin_add_overflow (*a, *b, r);
}

int
mul (unsigned *r, const unsigned *a, const unsigned *b)
{
  return __builtin_mul_overflow (*a, *b, r);
}

int
main ()
{
  unsigned x;

  /* 1073741824U + 1073741824U should not overflow.  */
  x = (__INT_MAX__ + 1U) / 2;
  if (add (&x, &x, &x))
    __builtin_abort ();

  /* 256U * 256U should not overflow */
  x = 1U << (sizeof (int) * __CHAR_BIT__ / 4);
  if (mul (&x, &x, &x))
    __builtin_abort ();

  /* 2147483648U + 2147483648U should overflow */
  x = __INT_MAX__ + 1U;
  if (!add (&x, &x, &x))
    __builtin_abort ();

  /* 65536U * 65536U should overflow */
  x = 1U << (sizeof (int) * __CHAR_BIT__ / 2);
  if (!mul (&x, &x, &x))
    __builtin_abort ();
}
