/* PR tree-optimization/122943 */

__attribute__((noipa)) unsigned char
foo (long long val)
{
  unsigned char result = 0;
  switch (val)
    {
    case 0: result = 1; break;
    case 1: result = 2; break;
    case 2: result = 3; break;
    default: break;
    }
  return result;
}

__attribute__((noipa)) unsigned char
bar (long long val)
{
  unsigned char result = 1;
  switch (val)
    {
    case 0: result = 8; break;
    case 1: result = 31; break;
    case 2: result = 72; break;
    default: break;
    }
  return result;
}

#ifdef __SIZEOF_INT128__
__attribute__((noipa)) unsigned char
baz (__int128 val)
{
  unsigned char result = 0;
  switch (val)
    {
    case 0: result = 1; break;
    case 1: result = 2; break;
    case 2: result = 3; break;
    default: break;
    }
  return result;
}

__attribute__((noipa)) unsigned char
qux (__int128 val)
{
  unsigned char result = 1;
  switch (val)
    {
    case 0: result = 8; break;
    case 1: result = 31; break;
    case 2: result = 72; break;
    default: break;
    }
  return result;
}
#endif

int
main ()
{
  if (foo (-1) != 0)
    __builtin_abort ();
  if (foo (0) != 1)
    __builtin_abort ();
  if (foo (1) != 2)
    __builtin_abort ();
  if (foo (2) != 3)
    __builtin_abort ();
  if (foo (3) != 0)
    __builtin_abort ();
  if (foo (-__LONG_LONG_MAX__ - 1) != 0)
    __builtin_abort ();
  if (foo (-__LONG_LONG_MAX__) != 0)
    __builtin_abort ();
  if (foo (-__LONG_LONG_MAX__ + 1) != 0)
    __builtin_abort ();
  if (bar (-1) != 1)
    __builtin_abort ();
  if (bar (0) != 8)
    __builtin_abort ();
  if (bar (1) != 31)
    __builtin_abort ();
  if (bar (2) != 72)
    __builtin_abort ();
  if (bar (3) != 1)
    __builtin_abort ();
  if (bar (-__LONG_LONG_MAX__ - 1) != 1)
    __builtin_abort ();
  if (bar (-__LONG_LONG_MAX__) != 1)
    __builtin_abort ();
  if (bar (-__LONG_LONG_MAX__ + 1) != 1)
    __builtin_abort ();
#ifdef __SIZEOF_INT128__
  if (baz (-1) != 0)
    __builtin_abort ();
  if (baz (0) != 1)
    __builtin_abort ();
  if (baz (1) != 2)
    __builtin_abort ();
  if (baz (2) != 3)
    __builtin_abort ();
  if (baz (3) != 0)
    __builtin_abort ();
  if (baz (((__int128) 1) << 64) != 0)
    __builtin_abort ();
  if (baz ((((__int128) 1) << 64) + 1) != 0)
    __builtin_abort ();
  if (baz ((((__int128) 1) << 64) + 2) != 0)
    __builtin_abort ();
  if (qux (-1) != 1)
    __builtin_abort ();
  if (qux (0) != 8)
    __builtin_abort ();
  if (qux (1) != 31)
    __builtin_abort ();
  if (qux (2) != 72)
    __builtin_abort ();
  if (qux (3) != 1)
    __builtin_abort ();
  if (qux (((__int128) 1) << 64) != 1)
    __builtin_abort ();
  if (qux ((((__int128) 1) << 64) + 1) != 1)
    __builtin_abort ();
  if (qux ((((__int128) 1) << 64) + 2) != 1)
    __builtin_abort ();
#endif
}
