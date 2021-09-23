/* PR tree-optimization/86723 */
/* { dg-do run { target { ilp32 || lp64 } } } */
/* { dg-options "-O2" } */

__attribute__((noipa)) int
foo (unsigned long long value)
{
  return (((value & 0x00000000000000ffull) << 56)
	  | ((value & 0x000000000000ff00ull) << 40)
	  | ((value & 0x0000000000ff0000ull) << 24)
	  | ((value & 0x00000000ff000000ull) << 8)
	  | ((value & 0x000000ff00000000ull) >> 8)
	  | ((value & 0x0000ff0000000000ull) >> 24)
	  | ((value & 0x00ff000000000000ull) >> 40)
	  | ((value & 0xff00000000000000ull) >> 56));
}

__attribute__((noipa)) int
bar (unsigned long long value)
{
  return (((value & 0x000000ff00000000ull) >> 8)
	  | ((value & 0x0000ff0000000000ull) >> 24)
	  | ((value & 0x00ff000000000000ull) >> 40)
	  | ((value & 0xff00000000000000ull) >> 56));
}

__attribute__((noipa)) unsigned long long
baz (unsigned long long value)
{
  return (((value & 0x00000000000000ffull) << 56)
	  | ((value & 0x000000000000ff00ull) << 40)
	  | ((value & 0x00000000ff000000ull) << 8)
	  | ((value & 0x000000ff00000000ull) >> 8)
	  | ((value & 0x0000ff0000000000ull) >> 24)
	  | ((value & 0xff00000000000000ull) >> 56));
}

__attribute__((noipa)) unsigned int
qux (unsigned int value)
{
  return (((value & 0x000000ff) << 24)
	  | ((value & 0x00ff0000) >> 8)
	  | ((value & 0xff000000) >> 24));
}

__attribute__((noipa)) unsigned int
corge (unsigned int value)
{
  return (((value & 0x000000ff) << 24)
	  | ((value & 0xff000000) >> 24));
}

int
main ()
{
  if (foo (0x0102030405060708ull) != 0x04030201
      || bar (0x0102030405060708ull) != 0x04030201
      || baz (0x0102030405060708ull) != 0x0807000504030001ull
      || qux (0x01020304) != 0x04000201
      || corge (0x01020304) != 0x04000001)
    __builtin_abort ();
  return 0;
}
