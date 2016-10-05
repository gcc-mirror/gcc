/* { dg-do run } */
/* { dg-require-effective-target split_stack } */
/* { dg-options "-fsplit-stack -O2" } */
/* { dg-options "-fsplit-stack -O2 -mno-accumulate-outgoing-args" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

/* A case that used to fail on 32-bit x86 when optimizing and not
   using -maccumulate-args.  The stack adjustment of the alloca got
   mixed up with the arguments pushed on the stack to the function
   before the call of alloca.  */

#include <stdlib.h>

typedef struct { const char* s; int l; } s;

typedef unsigned long long align16 __attribute__ ((aligned(16)));

s gobats (const void *, int) __attribute__ ((noinline));

s
gobats (const void* p __attribute__ ((unused)),
	int l __attribute__ ((unused)))
{
  s v;
  v.s = 0;
  v.l = 0;
  return v;
}

void check_aligned (void *p) __attribute__ ((noinline));

void
check_aligned (void *p)
{
  if (((__SIZE_TYPE__) p & 0xf) != 0)
    abort ();
}

void gap (void *) __attribute__ ((noinline));

void gap (void *p)
{
  align16 a;
  check_aligned (&a);
}

int
main (int argc, char **argv)
{
  s *space;
  gobats(0, 16);
  space = (s *) alloca(sizeof(s) + 1);
  *space = (s){0, 16};
  gap(space);
  return 0;
}
