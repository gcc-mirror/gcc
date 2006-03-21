/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

int f(_Bool a)
{
  int t = a;
  if (t != 2)
   return 0;
  return 1;
}

int f1(unsigned char a)
{
  int t = a;
  if (t != 256)
   return 0;
  return 1;
}

int f3 (unsigned char c)
{
  int i = c;
  if (i < 0 || i > 255)
    return -1;
  else
    return 0;
}

/* { dg-final { scan-tree-dump-times "if " 0 "vrp1" } } * /
/* { dg-final { cleanup-tree-dump "vrp1" } } */


