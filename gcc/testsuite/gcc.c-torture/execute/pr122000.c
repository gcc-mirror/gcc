/* PR target/122000 */

char c = 1;
__attribute__((aligned (sizeof (unsigned long long)))) unsigned long long ll;

int
main ()
{
#if defined(__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8) && __SIZEOF_LONG_LONG__ == 8 && __CHAR_BIT__ == 8
  unsigned long long x = __sync_add_and_fetch (&ll, c + 0xfedcba9876543210ULL);
  if (x != 0xfedcba9876543211ULL)
    __builtin_abort ();
#endif
}
