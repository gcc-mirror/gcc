typedef union longlong
{
  struct {unsigned short h0, h1, h2, h3;} h;
  struct {signed long low, high;} si;
  struct {unsigned long low, high;} ui;
  signed long long sll;
  unsigned long long ull;
} long_long;


long long
__negdi2 (u)
     long long u;
{
  long_long uu;

  uu.sll = u;

  uu.si.low = -uu.si.low;
  if (uu.si.low == 0)
    uu.si.high = -uu.si.high;
  else
    uu.si.high = ~uu.si.high;

  return uu.sll;
}
