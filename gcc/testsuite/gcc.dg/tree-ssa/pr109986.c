/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1 -Wno-psabi" } */

typedef int v4si __attribute__((vector_size(16)));

/* Generic */
__attribute__((noipa)) int 
t1 (int a, int b)
{
  return (~a | b) ^ a;
}

__attribute__((noipa)) unsigned int 
t2 (int a, int b)
{
  return a ^ (~a | (unsigned int) b);
}

__attribute__((noipa)) signed char
t3 (signed char a, signed char b)
{
  return (b | ~a) ^ a;
}

__attribute__((noipa)) unsigned char
t4 (signed char a, signed char b)
{
  return ((unsigned char) a) ^ (b | ~a);
}

__attribute__((noipa)) short
t5 (short a, short b)
{
  return a ^ (b | ~a);
}

__attribute__((noipa)) unsigned short
t6 (short a, short b)
{
  return ((unsigned short) a) ^ (b | ~a);
}

__attribute__((noipa)) long
t7 (long a, long b)
{
  return a ^ (b | ~a);
}

__attribute__((noipa)) unsigned long
t8 (long a, long b)
{
  return ((unsigned long) a) ^ (b | ~a);
}

__attribute__((noipa)) long long
t9 (long long a, long long b)
{
  return a ^ (b | ~a);
}

__attribute__((noipa)) unsigned long long
t10 (long long a, long long b)
{
  return ((unsigned long long) a) ^ (b | ~a);
}

__attribute__((noipa)) v4si
t21 (v4si a, v4si b)
{
  return a ^ (b | ~a);
}

/* Gimple */
__attribute__((noipa)) int 
t11 (int a, int b)
{
  int t1 = ~a;
  int t2 = t1 | b;
  int t3 = t2 ^ a;
  return t3;
}

__attribute__((noipa)) unsigned int
t12 (int a, unsigned int b)
{
  int t1 = ~a;
  unsigned int t2 = t1 | b;
  unsigned int t3 = a ^ t2;
  return t3;
}

__attribute__((noipa)) signed char
t13 (signed char a, signed char b)
{
  signed char t1 = ~a;
  signed char t2 = b | t1;
  signed char t3 = t2 ^ a;
  return t3;
}

__attribute__((noipa)) unsigned char
t14 (unsigned char a, signed char b)
{
  unsigned char t1 = ~a;
  signed char t2 = b | t1;
  unsigned char t3 = a ^ t2;
  return t3;
}

__attribute__((noipa)) short 
t15 (short a, short b)
{
  short t1 = ~a;
  short t2 = t1 | b;
  short t3 = t2 ^ a;
  return t3;
}

__attribute__((noipa)) unsigned short 
t16 (unsigned short a, short b)
{
  short t1 = ~a;
  short t2 = t1 | b;
  unsigned short t3 = t2 ^ a;
  return t3;
}

__attribute__((noipa)) long 
t17 (long a, long b)
{
  long t1 = ~a;
  long t2 = t1 | b;
  long t3 = t2 ^ a;
  return t3;
}

__attribute__((noipa)) unsigned long 
t18 (long a, unsigned long b)
{
  long t1 = ~a;
  unsigned long t2 = t1 | b;
  unsigned long t3 = t2 ^ a;
  return t3;
}

__attribute__((noipa)) long long 
t19 (long long a, long long b)
{
  long long t1 = ~a;
  long long t2 = t1 | b;
  long long t3 = t2 ^ a;
  return t3;
}

__attribute__((noipa)) unsigned long long 
t20 (long long a, long long b)
{
  long long t1 = ~a;
  long long t2 = t1 | b;
  unsigned long long t3 = a ^ t2;
  return t3;
}

__attribute__((noipa)) v4si
t22 (v4si a, v4si b)
{
  v4si t1 = ~a;
  v4si t2 = t1 | b;
  v4si t3 = a ^ t2;
  return t3;
}

/* { dg-final { scan-tree-dump-not " \\\| " "dse1" } } */
/* { dg-final { scan-tree-dump-not " \\\^ " "dse1" } } */
/* { dg-final { scan-tree-dump-times " ~" 22 "dse1" } } */
/* { dg-final { scan-tree-dump-times " & " 22 "dse1" } } */

