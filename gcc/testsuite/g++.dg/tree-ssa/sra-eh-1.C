/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-sra" } */

struct E
{
  short c;
  E (short param) : c(param) {};
};

volatile short vs = 0;
long buf[16];
long * volatile pbuf = (long *) &buf;

static void __attribute__((noinline))
unrelated_throwing (void)
{
  throw E (256);
}

struct S {
  long  *b;
  int c;
  int s;
};

static void __attribute__((noinline))
crazy_alloc_s (struct S *p)
{
  int s = p->c ? p->c * 2 : 16;

  long *b = pbuf;
  if (!b || s > 16)
    {
      p->s = -p->s;
      throw E (127);
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

void __attribute__((noipa))
check (int s, short c)
{
  if (vs < c)
    vs = c;
  if (c == 127)
    {
      if (s >= 0)
	__builtin_abort ();
    }
  else
    {
      if (s != 0)
	__builtin_abort ();
    }
  return;
}


void
foo (void)
{
  struct S foo_stack;

  foo_stack.c = 0;
  crazy_alloc_s (&foo_stack);
  foo_stack.b[0] = 1;
  foo_stack.c = 1;

  while (foo_stack.c)
    {
      long l = foo_stack.b[--foo_stack.c];

      if (l > 0)
	{
	  for (int i = 0; i < 4; i++)
	    {
	      try
		{
		  if (foo_stack.s <= foo_stack.c + 1)
		    crazy_alloc_s (&foo_stack);
		}
	      catch (E e)
		{
		  check (foo_stack.s, e.c);
		  return;
		}
	      l = process (l);
	      foo_stack.b[foo_stack.c++] = l;
	    }
	}
    }
  return;
}


volatile int vi;

int __attribute__((noipa))
save (int s)
{
  vi = s;
  return 0;
}

int __attribute__((noipa))
restore ()
{
  return vi;
}


void
bar (void)
{
  struct S bar_stack;

  bar_stack.c = 0;
  crazy_alloc_s (&bar_stack);
  bar_stack.b[0] = 1;
  bar_stack.c = 1;

  while (bar_stack.c)
    {
      long l = bar_stack.b[--bar_stack.c];

      if (l > 0)
	{
	  for (int i = 0; i < 4; i++)
	    {
	      try
		{
		  if (i == 2)
		    {
		      bar_stack.s = save (bar_stack.s);
		      unrelated_throwing ();
		    }
		  if (bar_stack.s <= bar_stack.c + 1)
		    crazy_alloc_s (&bar_stack);
		}
	      catch (E e)
		{
		  check (bar_stack.s, e.c);
		  if (e.c == 127)
		    return;
		  bar_stack.s = restore ();
		  continue;
		}
	      l = process (l);
	      bar_stack.b[bar_stack.c++] = l;
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

  vs = 0;
  pbuf = (long *) &buf;
  bar ();
  if (vs != 256)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "Created a replacement for foo_stack offset" "sra"} } */
/* { dg-final { scan-tree-dump "Created a replacement for bar_stack offset" "sra"} } */
