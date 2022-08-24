__attribute__((noinline))
__int128 multiply_add (long a, long b, long c)
{
  return (__int128) a * b + c;
}

__attribute__((noinline))
unsigned __int128 multiply_addu (unsigned long a, unsigned long b,
				 unsigned long c)
{
  return (unsigned __int128) a * b + c;
}
