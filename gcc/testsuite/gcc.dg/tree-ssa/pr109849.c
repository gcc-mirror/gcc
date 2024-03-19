/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-sra" } */

struct D
{
  long a;
  short b;
};

struct S {
  struct D *b;
  int c;
  int s;
};

struct D * __attribute__((malloc)) some_realloc_function (int c);

static void __attribute__((noinline))
realloc_s (struct S *p)
{
  int s = p->c ? p->c * 2 : 16;
  p->b = some_realloc_function (s);
  p->s = s;
  return;
}

void modify (struct D *d);
int test (struct D *d);

static struct D gd;

int
foo (void)
{
  struct S stack;

  stack.c = 1;
  stack.s = 0;
  realloc_s (&stack);
  stack.b[0] = gd;
  stack.c = 1;

  while (stack.c)
    {
      struct D d = stack.b[--stack.c];
      if (test (&d))
	{
	  for (int i = 0; i < 8; i++)
	    {
	      if (stack.s <= stack.c + 1)
		realloc_s (&stack);
	      modify(&d);
	      stack.b[stack.c++] = d;
	    }
	}
    }
  return 0;
}

/* { dg-final { scan-tree-dump "Created a replacement for stack offset" "sra"} } */
