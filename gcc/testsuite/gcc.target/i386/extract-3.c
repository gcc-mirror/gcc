/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic" } */

typedef struct
{
  unsigned char c1;
  unsigned char c2;
  unsigned char c3;
  unsigned char c4;
} foo_t;

int
#ifndef __x86_64__
__attribute__((regparm(3)))
#endif
foo (foo_t x)
{
   return x.c2 != 0;
}

/* { dg-final { scan-assembler-not "test\[b\]?\[^\\n\]*%\[a-z0-9\]+l" } } */
