/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic" } */

typedef struct
{
  unsigned int c1:8;
  unsigned int c2:8;
  unsigned int c3:8;
  unsigned int c4:8;
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
