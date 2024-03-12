/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse2 -Wno-psabi" } */

typedef int v4si __attribute__((vector_size(4 * sizeof(int))));

/* Generic */
__attribute__((noipa)) int
t1 (int a, int b, int c)
{
  return ((a ^ c) & b) | a;
}

__attribute__((noipa)) unsigned int
t2 (int a, unsigned int b, int c)
{
  return ((a ^ c) & b) | a;
}

__attribute__((noipa)) unsigned long
t3 (unsigned long a, long b, unsigned long c)
{
  return ((a ^ c) & b) | a;
}

__attribute__((noipa)) unsigned short
t4 (short a, unsigned short b, unsigned short c)
{
  return (unsigned short) ((a ^ c) & b) | a;
}

__attribute__((noipa)) unsigned char
t5 (unsigned char a, signed char b, signed char c)
{
  return ((a ^ c) & b) | a;
}

__attribute__((noipa)) long long
t6 (long long a, long long b, long long c)
{
  return ((a ^ c) & (unsigned long long) b) | a;
}

/* Gimple */
__attribute__((noipa)) int
t7 (int a, int b, int c)
{
  int t1 = a ^ c;
  int t2 = t1 & b;
  int t3 = t2 | a;
  return t3;
}

__attribute__((noipa)) int
t8 (int a, unsigned int b, unsigned int c)
{
  unsigned int t1 = a ^ c;
  int t2 = t1 & b;
  int t3 = t2 | a;
  return t3;
}

__attribute__((noipa)) unsigned int
t9 (unsigned int a, unsigned int b, int c)
{
  unsigned int t1 = a ^ c;
  unsigned int t2 = t1 & b;
  unsigned int t3 = t2 | a;
  return t3;
}

__attribute__((noipa)) unsigned long
t10 (unsigned long a, long b, unsigned long c)
{
  unsigned long t1 = a ^ c;
  unsigned long t2 = t1 & b;
  unsigned long t3 = t2 | a;
  return t3;
}

__attribute__((noipa)) unsigned short
t11 (short a, unsigned short b, short c)
{
  short t1 = a ^ c;
  unsigned short t2 = t1 & b;
  unsigned short t3 = t2 | a;
  return t3;
}

__attribute__((noipa)) unsigned char
t12 (signed char a, unsigned char b, signed char c)
{
  unsigned char t1 = a ^ c;
  unsigned char t2 = t1 & b;
  unsigned char t3 = t2 | a;
  return t3;
}

__attribute__((noipa)) unsigned long long
t13 (unsigned long long a, long long b, unsigned long long c)
{
  long long t1 = a ^ c;
  long long t2 = t1 & b;
  unsigned long long t3 = t2 | a;
  return t3;
}

/* Vectors */
__attribute__((noipa)) v4si
t14 (v4si a, v4si b, v4si c)
{
  return ((a ^ c) & b) | a;
}

__attribute__((noipa)) v4si
t15 (v4si a, v4si b, v4si c)
{
  v4si t1 = a ^ c;
  v4si t2 = t1 & b;
  v4si t3 = t2 | a;
  return t3;
}

/* { dg-final { scan-tree-dump-not " \\\^ " "dse2" } } */
/* { dg-final { scan-tree-dump-times " \\\| " 15 "dse2" } } */
/* { dg-final { scan-tree-dump-times " & " 15 "dse2" } } */
