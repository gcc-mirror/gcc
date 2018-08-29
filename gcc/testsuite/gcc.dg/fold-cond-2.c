/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
min1 (signed char op1, signed char op2)
{
  return (op1 < 25) ? (int)op1 : 24;
}
int
min2 (signed char op1, signed char op2)
{
  return (op1 <= 24) ? (int)op1 : 25;
}
int
min3 (unsigned char op1, unsigned char op2)
{
  return (op1 < 25) ? (unsigned int)op1 : 24;
}
int
min4 (unsigned char op1, unsigned char op2)
{
  return (op1 <= 24) ? (unsigned int)op1 : 25;
}
int
max1 (signed char op1, signed char op2)
{
  return (op1 > 24) ? (int)op1 : 25;
}
int
max2 (signed char op1, signed char op2)
{
  return (op1 >= 25) ? (int)op1 : 24;
}
int
max3 (unsigned char op1, unsigned char op2)
{
  return (op1 > 24) ? (unsigned int)op1 : 25;
}
int
max4 (unsigned char op1, unsigned char op2)
{
  return (op1 >= 25) ? (unsigned int)op1 : 24;
}

/* { dg-final { scan-tree-dump-times "MIN_EXPR" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 4 "optimized" } } */

