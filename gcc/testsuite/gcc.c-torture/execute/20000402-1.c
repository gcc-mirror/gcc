void abort(void);
void exit(int);

#include <limits.h>

#if ULONG_LONG_MAX != 18446744073709551615ull && ULONG_MAX != 18446744073709551615ull
int main(void) { exit (0); }
#else
#if ULONG_MAX != 18446744073709551615ull
typedef unsigned long long ull;
#else
typedef unsigned long ull;
#endif

#include <stdio.h>

void checkit(int);

int main (void) {
    const ull a = 0x1400000000ULL;
    const ull b = 0x80000000ULL;
    const ull c = a/b;
    const ull d = 0x1400000000ULL / 0x80000000ULL;

    checkit ((int) c);
    checkit ((int) d);

    exit(0);
}

void checkit (int a)
{
  if (a != 40)
    abort();
}
#endif
