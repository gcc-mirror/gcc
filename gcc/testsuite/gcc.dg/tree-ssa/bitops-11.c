/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized-raw -Wno-psabi" } */

typedef int v4si __attribute__((vector_size(4 * sizeof(int))));

/* Generic */
#define BIT_OPERATIONS(result_type, operand_type, suffix) \
result_type foo_generic_1##suffix(operand_type a) {       \
    return (a - 1) & -a;                                  \
}                                                         \
result_type foo_generic_2##suffix(operand_type a) {       \
    return (a - 1) | -a;                                  \
}                                                         \
result_type foo_generic_3##suffix(operand_type a) {       \
    return (a - 1) ^ -a;                                  \
}

BIT_OPERATIONS(signed char, signed char, 11)
BIT_OPERATIONS(unsigned char, unsigned char, 12)

BIT_OPERATIONS(signed short, signed short, 21)
BIT_OPERATIONS(unsigned short, unsigned short, 22)

BIT_OPERATIONS(signed int, signed int, 31)
BIT_OPERATIONS(unsigned int, unsigned int, 32)

BIT_OPERATIONS(signed long, signed long, 41)
BIT_OPERATIONS(unsigned long, unsigned long, 42)

/* Gimple */
int
foo_gimple_1(int a)
{
  int t1 = a - 1; 
  int t2 = -a;
  int t3 = t1 & t2;
  return t3;
}

short
foo_gimple_2(short a)
{
  short t1 = a - 1; 
  short t2 = -a;
  short t3 = t1 | t2;
  return t3;
}

unsigned long
foo_gimple_3(unsigned long a)
{
  unsigned long t1 = a - 1;
  unsigned long t2 = -a;
  unsigned long t3 = t1 ^ t2;
  return t3;
}

int
foo_gimple_4(int a, unsigned char b)
{
  /* The return expression should simplify to b + 7.  */
  int t1 = b;
  t1 |= (a - 1) | -a;
  t1 |= b & (a >> 3);

  int t2 = b + 7;
  t2 &= ~((b - 1) & -b);
  t2 &= (a - 1) ^ -a;

  int t3 = t1 & t2;
  return t3;
}

/* Vectors */
v4si
foo_vector_1(v4si a)
{
  return (a - (v4si) {1, 1, 1, 1}) & -a;
}

v4si
foo_vector_2(v4si a)
{
  v4si t0 = (v4si) {1, 1, 1, 1};
  v4si t1 = a - t0;
  v4si t2 = -a;
  v4si t3 = t1 | t2;
  return t3;
}

v4si
foo_vector_3(v4si a)
{
  v4si t0 = (v4si) {1, 1, 1, 1};
  v4si t1 = a - t0;
  v4si t2 = -a;
  v4si t3 = t1 ^ t2;
  return t3;
}

v4si
foo_vector_4(v4si a)
{
  v4si t0 = (v4si) {1, 1, 1, 1};
  v4si t1 = (a - t0) & -a;
  v4si t2 = (a - t0) | -a;
  v4si t3 = (a - t0) ^ -a;
  v4si t4 = t1 - t2 + t3;
  return t4;
}

/* { dg-final { scan-tree-dump-not   "bit_and_expr, "   "optimized" } } */
/* { dg-final { scan-tree-dump-not   "bit_ior_expr, "   "optimized" } } */
/* { dg-final { scan-tree-dump-not   "bit_xor_expr, "   "optimized" } } */
/* { dg-final { scan-tree-dump-not   "negate_expr, "    "optimized" } } */
/* { dg-final { scan-tree-dump-times "plus_expr, " 1 "optimized" } } */

