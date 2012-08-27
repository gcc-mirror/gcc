/* { dg-do run } */
/* { dg-options "-mgp64" } */
typedef int int128_t __attribute__((mode(TI)));
typedef unsigned int uint128_t __attribute__((mode(TI)));

#define UINT128_CONST(A, B) \
  (((uint128_t) (0x ## A ## ULL) << 64) | (0x ## B ## ULL))

volatile uint128_t a = UINT128_CONST (1111111111111111, a222222222222222);
volatile uint128_t b = UINT128_CONST (0000000000000005, 0000000000000003);
volatile uint128_t c = UINT128_CONST (5dddddddddddddde, e666666666666666);
volatile uint128_t d = UINT128_CONST (e612340000000000, 5000000000234500);
volatile uint128_t e = UINT128_CONST (43f011dddddddddf, 366666666689ab66);
volatile uint128_t f = UINT128_CONST (4210100000000000, 1000000000010100);
volatile uint128_t g = UINT128_CONST (a5e225dddddddddf, 6666666666aaee66);
volatile uint128_t h = UINT128_CONST (e7f235dddddddddf, 7666666666abef66);
volatile uint128_t i = UINT128_CONST (5e225dddddddddf6, 666666666aaee660);
volatile uint128_t j = UINT128_CONST (0a5e225ddddddddd, f6666666666aaee6);
volatile uint128_t k = UINT128_CONST (fa5e225ddddddddd, f6666666666aaee6);

volatile int amount = 4;

volatile uint128_t result;

int
test1 (void)
{
  result = a * b;
  if (result != c)
    return 1;
  return 0;
}

int
test2 (void)
{
  result = c + d;
  if (result != e)
    return 1;
  return 0;
}

int
test3 (void)
{
  result = e - d;
  if (result != c)
    return 1;
  return 0;
}

int
test4 (void)
{
  result = d & e;
  if (result != f)
    return 1;
  return 0;
}

int
test5 (void)
{
  result = d ^ e;
  if (result != g)
    return 1;
  return 0;
}

int
test6 (void)
{
  result = d | e;
  if (result != h)
    return 1;
  return 0;
}

int
test7 (void)
{
  result = g << amount;
  if (result != i)
    return 1;
  return 0;
}

int
test8 (void)
{
  result = g >> amount;
  if (result != j)
    return 1;
  return 0;
}

int
test9 (void)
{
  result = (int128_t) g >> amount;
  if (result != k)
    return 1;
  return 0;
}

int
main (void)
{
  return (test1 ()
	  | test2 ()
	  | test3 ()
	  | test4 ()
	  | test5 ()
	  | test6 ()
	  | test7 ()
	  | test8 ()
	  | test9 ());
}
