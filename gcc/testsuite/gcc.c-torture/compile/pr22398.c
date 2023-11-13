#if ULONG_MAX != 4294967295u && ULONG_MAX != 18446744073709551615ull
int main(void) { __builtin_exit (0); }
#else
#if ULONG_MAX != 18446744073709551615ull
#define NUM 0xf0000000
#else
#define NUM 0xf000000000000000
#endif


int func1(void *rw)
{
  return (rw && (((unsigned long) rw) >= NUM) );
}

void func2(void *rw)
{
  while(rw && (((unsigned long) rw) >= NUM) ) {}
}

#endif
