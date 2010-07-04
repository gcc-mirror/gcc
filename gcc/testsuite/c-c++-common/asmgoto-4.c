/* PR middle-end/44071 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

static inline int
f1 (void)
{
  asm goto ("" : : : : l1, l2);
  __builtin_unreachable ();
 l1:
  return 1;
 l2:
  return 0;
}

int
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
  asm goto ("" : : : : l1, l2);
 l1:
  return 1;
 l2:
  return 0;
}

int
b2 (int x)
{
  if (f2 () || x == 6)
    x = 1;
  else
    x = 2;
  return x;
}
