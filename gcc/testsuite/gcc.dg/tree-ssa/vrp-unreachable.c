/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1 -fdump-tree-vrp2 -fdump-tree-optimized-alias" } */

void dead (unsigned n);
void alive (unsigned n);

void func (unsigned n, unsigned m)
{
  if (n == 0)
    __builtin_unreachable();
  if (n == 1)
    __builtin_unreachable();
  if (n & 0x1)
    __builtin_unreachable();
  if (n == 2)
    __builtin_unreachable();
  if (n == 3)
    __builtin_unreachable();
  if (n & 0x2)
    __builtin_unreachable();
  if (n == 4)
    __builtin_unreachable();
  if (n == 5)
    __builtin_unreachable();
  if (n & 0x4)
    __builtin_unreachable();
  if (n == 6)
    __builtin_unreachable();
  if (n == 7)
    __builtin_unreachable();
 if (n <8)
   dead (n);
 if (n != m)
    __builtin_unreachable();
 alive (n);
 alive (m);
}

/* { dg-final { scan-tree-dump-not "dead" "vrp1" } } */
/* { dg-final { scan-tree-dump-times "builtin_unreachable" 1 "vrp1" } } */
/* { dg-final { scan-tree-dump-not "builtin_unreachable" "vrp2" } } */
/* { dg-final { scan-tree-dump-times "fff8 VALUE 0x0" 2 "optimized" } } */
