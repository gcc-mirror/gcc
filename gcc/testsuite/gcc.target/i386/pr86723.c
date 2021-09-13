/* PR tree-optimization/86723 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "\tbswap\t" 5 } } */

int
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

int
bar (unsigned long long value)
{
  return (((value & 0x000000ff00000000ull) >> 8)
	  | ((value & 0x0000ff0000000000ull) >> 24)
	  | ((value & 0x00ff000000000000ull) >> 40)
	  | ((value & 0xff00000000000000ull) >> 56));
}

unsigned long long
baz (unsigned long long value)
{
  return (((value & 0x00000000000000ffull) << 56)
	  | ((value & 0x000000000000ff00ull) << 40)
	  | ((value & 0x00000000ff000000ull) << 8)
	  | ((value & 0x000000ff00000000ull) >> 8)
	  | ((value & 0x0000ff0000000000ull) >> 24)
	  | ((value & 0xff00000000000000ull) >> 56));
}

unsigned int
qux (unsigned int value)
{
  return (((value & 0x000000ff) << 24)
	  | ((value & 0x00ff0000) >> 8)
	  | ((value & 0xff000000) >> 24));
}

unsigned int
corge (unsigned int value)
{
  return (((value & 0x000000ff) << 24)
	  | ((value & 0xff000000) >> 24));
}
