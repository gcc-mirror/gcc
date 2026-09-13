/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1" } */
/* { dg-require-effective-target int32 } */

int
g1 (int x)
{
  int a = x + 5;
  int b = 5 - x;
  int c = a | b;
  if (c < 0)
    return 1;
  return 0;
}

unsigned int
g2 (int x)
{
  int a = x + 5;
  int b = 5 - x;
  unsigned int c = (unsigned int) (a | b);
  return c >> 31;
}

int
g3 (int c)
{
  int a = c - 'a';
  int b = 'z' - c;
  int d = a | b;
  if (d < 0)
    return 0;
  return 1;
}

int
g4 (int x)
{
  int a = __builtin_abs (x);
  if (a > 5)
    return 1;
  return 0;
}

int
g5 (int x)
{
  int a = 6 - x;
  int b = x | a;
  if (b < 0)
    return 1;
  return 0;
}

int
g6 (int x)
{
  int a = x + 6;
  int b = -x;
  int c = a | b;
  if (c < 0)
    return 1;
  return 0;
}

int
g7 (int x)
{
  int a = -x;
  int b = x | a;
  if (b < 0)
    return 1;
  return 0;
}

int
g8 (int x)
{
  int a = __builtin_abs (x);
  if (a <= 5)
    return 1;
  return 0;
}

/* { dg-final { scan-tree-dump-not " \\| " "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "ABS_EXPR" "forwprop1" } } */
/* { dg-final { scan-tree-dump-times { > 10[;)]} 3 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times { <= 10[;)]} 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times { > 25[;)]} 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times { > 6[;)]} 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times { != 0[;)]} 1 "forwprop1" } } */
