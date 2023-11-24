/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-sra" } */
/* { dg-require-effective-target indirect_jumps } */

#include <setjmp.h>

struct S {
  long *b;
  int c;
  int s;
};

static jmp_buf the_jmpbuf;
volatile short vs = 0;
long buf[16];
long * volatile pbuf = (long *) &buf;

static void __attribute__((noinline))
crazy_alloc_s (struct S *p)
{
  int s = p->c ? p->c * 2 : 16;

  long *b = pbuf;
  if (!b || s > 16)
    {
      p->s = -p->s;
      vs = 127;
      longjmp (the_jmpbuf, 1);
    }

  __builtin_memcpy (b, p->b, p->c);
  p->b = b;
  p->s = s;
  pbuf = 0;
  return;
}

long __attribute__((noipa))
process (long v)
{
  return v + 1;
}

void
foo (void)
{
  struct S stack;

  if (setjmp (the_jmpbuf))
    return;

  stack.c = 0;
  crazy_alloc_s (&stack);
  stack.b[0] = 1;
  stack.c = 1;

  while (stack.c)
    {
      long l = stack.b[--stack.c];

      if (l > 0)
	{
	  for (int i = 0; i < 4; i++)
	    {
	      if (stack.s <= stack.c + 1)
		crazy_alloc_s (&stack);
	      l = process (l);
	      stack.b[stack.c++] = l;
	    }
	}
    }

  return;
}

int main (int argc, char **argv)
{
  vs = 0;
  pbuf = (long *) &buf;
  foo ();
  if (vs != 127)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-not "Created a replacement for stack offset" "sra"} } */
