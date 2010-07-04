/* PR middle-end/44071 */
/* { dg-do run } */
/* { dg-options "-O2" } */

static inline int
f1 (void)
{
  asm goto ("jmp %l[l1]" : : : : l1, l2);
  __builtin_unreachable ();
 l1:
  return 1;
 l2:
  return 0;
}

__attribute__((noinline)) int
b1 (int x)
{
  if (f1 () || x == 6)
    x = 1;
  else
    x = 2;
  return x;
}

static inline int
f2 (void)
{
  asm goto ("jmp %l[l2]" : : : : l1, l2);
  __builtin_unreachable ();
 l1:
  return 1;
 l2:
  return 0;
}

__attribute__((noinline)) int
b2 (int x)
{
  if (f2 () || x == 6)
    x = 1;
  else
    x = 2;
  return x;
}

static inline int
f3 (void)
{
  asm goto ("jmp %l[l1]" : : : : l1, l2);
 l1:
  return 1;
 l2:
  return 0;
}

__attribute__((noinline)) int
b3 (int x)
{
  if (f3 () || x == 6)
    x = 1;
  else
    x = 2;
  return x;
}

static inline int
f4 (void)
{
  asm goto ("jmp %l[l2]" : : : : l1, l2);
 l1:
  return 1;
 l2:
  return 0;
}

__attribute__((noinline)) int
b4 (int x)
{
  if (f4 () || x == 6)
    x = 1;
  else
    x = 2;
  return x;
}

extern void abort (void);

int
main (void)
{
  int x;
  asm ("" : "=r" (x) : "0" (0));
  if (b1 (x) != 1 || b1 (x + 6) != 1)
    abort ();
  if (b2 (x) != 2 || b2 (x + 6) != 1)
    abort ();
  if (b3 (x) != 1 || b3 (x + 6) != 1)
    abort ();
  if (b4 (x) != 2 || b4 (x + 6) != 1)
    abort ();
  return 0;
}
