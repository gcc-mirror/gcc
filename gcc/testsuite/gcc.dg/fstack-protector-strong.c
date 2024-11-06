/* Test that stack protection is done on chosen functions. */

/* { dg-do compile { target i?86-*-* x86_64-*-* rs6000-*-* s390x-*-* } } */
/* { dg-options "-O2 -fstack-protector-strong" } */

/* This test checks the presence of __stack_chk_fail function in assembler.
 * Compiler generates _stack_chk_fail_local (wrapper) calls instead for PIC.
 */
/* { dg-require-effective-target nonpic } */

#include<string.h>

extern int g0;
extern int* pg0;
int
goo (int *);
int
hoo (int);

/* Function frame address escaped function call. */
int
foo1 ()
{
  int i;
  return goo (&i);
}

struct ArrayStruct
{
  int a;
  int array[10];
};

struct AA
{
  int b;
  struct ArrayStruct as;
};

/* Function frame contains array. */
int
foo2 ()
{
  struct AA aa;
  int i;
  for (i = 0; i < 10; ++i)
    {
      aa.as.array[i] = i * (i-1) + i / 2;
    }
  return aa.as.array[5];
}

/* Address computation based on a function frame address. */
int
foo3 ()
{
  int a;
  int *p;
  p = &a + 5;
  return goo (p);
}

/* Address cast based on a function frame address. */
int
foo4 ()
{
  int a;
  return goo (g0 << 2 ? (int *)(3 * (long)(void *)(&a)) : 0);
}

/* Address cast based on a local array. */
int
foo5 ()
{
  short array[10];
  return goo ((int *)(array + 5));
}

struct BB
{
  int one;
  int two;
  int three;
};

/* Address computaton based on a function frame address.*/
int
foo6 ()
{
  struct BB bb;
  return goo (&bb.one + sizeof(int));
}

/* Function frame address escaped via global variable. */
int
foo7 ()
{
  int a;
  pg0 = &a;
  goo (pg0);
  return *pg0;
}

/* Check that this covers -fstack-protector. */
int
foo8 ()
{
  char base[100];
  memcpy ((void *)base, (const void *)pg0, 105);   /* { dg-warning "writing 105 bytes into a region of size 100" } */
  return (int)(base[32]);
}

/* Check that this covers -fstack-protector. */
int
foo9 ()
{
  char* p = __builtin_alloca (100);
  return goo ((int *)(p + 50));
}

int
global2 (struct BB* pbb);

/* Address taken on struct. */
int
foo10 ()
{
  struct BB bb;
  int i;
  bb.one = global2 (&bb);
  for (i = 0; i < 10; ++i)
    {
      bb.two = bb.one + bb.two;
      bb.three = bb.one + bb.two + bb.three;
    }
  return bb.three;
}

struct B
{
  /* Discourage passing this struct in registers. */
  int a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
};

struct B global3 (void);

int foo11 ()
{
  return global3 ().a1;
}

void foo12 ()
{
  global3 ();
}

/* { dg-final { scan-assembler-times "stack_chk_fail" 11 } } */
