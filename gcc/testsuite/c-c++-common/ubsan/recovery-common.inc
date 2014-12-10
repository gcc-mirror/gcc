typedef const unsigned long long int CULLI;
typedef volatile int VI;
struct s { signed long int a; };

int
main (void)
{
  volatile int shiftcount = 153;
  volatile int a = __INT_MAX__;
  volatile int b = __INT_MAX__;

  a << 152;
  b << shiftcount;
  a += 1;
  b += 2;

  return 0;
}

