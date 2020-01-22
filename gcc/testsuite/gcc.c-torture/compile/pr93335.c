/* PR target/93335 */
/* { dg-do compile { target int128 } } */

int
f1 (unsigned int x)
{
  return __builtin_sub_overflow_p (x, 4096, (unsigned __int128) 0);
}

int
f2 (unsigned int x)
{
  return __builtin_sub_overflow_p (x, 4097, (unsigned __int128) 0);
}

int
f3 (int x)
{
  return __builtin_sub_overflow_p (x, 4096, (__int128) 0);
}

int
f4 (int x)
{
  return __builtin_sub_overflow_p (x, 4097, (__int128) 0);
}

int
f5 (unsigned int x)
{
  return __builtin_sub_overflow_p (x, -4096, (unsigned __int128) 0);
}

int
f6 (unsigned int x)
{
  return __builtin_sub_overflow_p (x, -4097, (unsigned __int128) 0);
}

int
f7 (int x)
{
  return __builtin_sub_overflow_p (x, -4096, (__int128) 0);
}

int
f8 (int x)
{
  return __builtin_sub_overflow_p (x, -4097, (__int128) 0);
}

int
f9 (unsigned int x)
{
  return __builtin_add_overflow_p (x, 4096, (unsigned __int128) 0);
}

int
f10 (unsigned int x)
{
  return __builtin_add_overflow_p (x, 4097, (unsigned __int128) 0);
}

int
f11 (int x)
{
  return __builtin_add_overflow_p (x, 4096, (__int128) 0);
}

int
f12 (int x)
{
  return __builtin_add_overflow_p (x, 4097, (__int128) 0);
}

int
f13 (unsigned int x)
{
  return __builtin_add_overflow_p (x, -4096, (unsigned __int128) 0);
}

int
f14 (unsigned int x)
{
  return __builtin_add_overflow_p (x, -4097, (unsigned __int128) 0);
}

int
f15 (int x)
{
  return __builtin_add_overflow_p (x, -4096, (__int128) 0);
}

int
f16 (int x)
{
  return __builtin_add_overflow_p (x, -4097, (__int128) 0);
}
