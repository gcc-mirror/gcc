/* { dg-do run } */
/* { dg-options { -std=c99 } } */

typedef _Bool bool;
typedef __UINT8_TYPE__ uint8_t;

static inline __attribute__((__always_inline__))
bool func1a (bool p1, uint8_t p2)
{
  if (p1)
    return p2 <= 8;
  return p2 <= 2;
}

__attribute__((__noipa__))
bool func1b (bool p1, uint8_t p2)
{
  return func1a (p1, p2);
}

static inline __attribute__((__always_inline__))
bool func2a (bool p1, unsigned p2)
{
  if (p1)
    return p2 <= 8;
  return p2 <= 2;
}

__attribute__((__noipa__))
bool func2b (bool p1, unsigned p2)
{
  return func2a (p1, p2);
}

void test1 (void)
{
  if (func1a (0, 1) != func1b (0, 1)) __builtin_abort();
  if (func1a (0, 2) != func1b (0, 2)) __builtin_abort();
  if (func1a (0, 3) != func1b (0, 3)) __builtin_abort();

  if (func1a (1, 7) != func1b (1, 7)) __builtin_abort();
  if (func1a (1, 8) != func1b (1, 8)) __builtin_abort();
  if (func1a (1, 9) != func1b (1, 9)) __builtin_abort();
}

void test2 (void)
{
  if (func2a (0, 1) != func2b (0, 1)) __builtin_abort();
  if (func2a (0, 2) != func2b (0, 2)) __builtin_abort();
  if (func2a (0, 3) != func2b (0, 3)) __builtin_abort();

  if (func2a (1, 7) != func2b (1, 7)) __builtin_abort();
  if (func2a (1, 8) != func2b (1, 8)) __builtin_abort();
  if (func2a (1, 9) != func2b (1, 9)) __builtin_abort();
}

int main (void)
{
  test1();
  test2();

  __builtin_exit (0);
}
