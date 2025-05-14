/* Adapted from malloc-1.c, but wrapping the pointers in a struct.  */

#include <stdlib.h>

extern int foo (void);
extern int bar (void);
extern void could_free (void *);
extern void cant_free (const void *); /* since it's a const void *.  */

typedef struct boxed_ptr { void *value; } boxed_ptr;

boxed_ptr
boxed_malloc (size_t sz)
{
  boxed_ptr result;
  result.value = malloc (sz);
  return result;
}

boxed_ptr
boxed_free (boxed_ptr ptr)
{
  free (ptr.value);
}

const boxed_ptr boxed_null = {NULL};

void test_1 (void)
{
  boxed_ptr ptr;
  ptr.value = malloc (1024);
  free (ptr.value);
  free (ptr.value); /* { dg-warning "double-'free' of 'ptr.value'" } */
}

void test_2 (boxed_ptr ptr)
{
  free (ptr.value);
  free (ptr.value); /* { dg-warning "double-'free' of 'ptr.value'" } */
}

boxed_ptr
test_3 (void)
{
  boxed_ptr ptr;
  ptr.value = malloc (sizeof (int));
  *(int *)ptr.value = 42; /* { dg-warning "dereference of possibly-NULL 'ptr.value' \\\[CWE-690\\\]" } */
  return ptr;
}

boxed_ptr
test_4 (void)
{
  boxed_ptr ptr;
  ptr.value = malloc (sizeof (int));
  int *iptr = (int *)ptr.value;
  if (iptr)
    *iptr = 42;
  else
    *iptr = 43; /* { dg-warning "dereference of NULL 'iptr' \\\[CWE-476\\\]" } */
  return ptr;
}

int test_5 (boxed_ptr ptr)
{
  free (ptr.value);
  return *(int *)ptr.value; /* { dg-warning "use after 'free' of 'ptr.value'" } */
}

void test_6 (void *ptr)
{
  boxed_ptr q;
  q.value = ptr;
  free (ptr);
  free (q.value); /* { dg-warning "double-'free' of 'ptr'" } */
}

void test_6a (boxed_ptr ptr)
{
  boxed_ptr q;
  q = ptr;
  boxed_free (ptr);
  free (q.value); /* { dg-warning "double-'free' of 'ptr.value'" } */
}

void test_7 (void)
{
  boxed_ptr ptr = boxed_malloc(4096);
  if (!ptr.value)
    return;
  __builtin_memset(ptr.value, 0, 4096);
  boxed_free(ptr);
}

boxed_ptr test_8 (void)
{
  boxed_ptr ptr = boxed_malloc(4096);
  if (!ptr.value)
    return boxed_null;
  __builtin_memset(ptr.value, 0, 4096);
  return ptr;
}

void test_9 (void)
{
  boxed_ptr ptr = boxed_malloc (1024);

  int i;
  for (i = 0; i < 1024; i++)
    free (ptr.value); /* { dg-warning "double-'free' of 'ptr.value'" } */
}

void test_10 (void)
{
  boxed_ptr ptr = boxed_malloc (1024);

  int i;
  for (i = 0; i < 1024; i++)
    foo ();

  free (ptr.value);
  free (ptr.value); /* { dg-warning "double-'free' of 'ptr.value'" } */ 
}

void test_11 (void)
{
  boxed_ptr ptr = boxed_malloc (1024);

  while (foo ())
    bar ();

  free (ptr.value);
  free (ptr.value); /* { dg-warning "double-'free' of 'ptr.value'" } */
}

void test_12 (void)
{
  boxed_ptr ptr = boxed_malloc (1024);

  while (1)
    {
      free (ptr.value); /* { dg-warning "infinite loop" } */
      free (ptr.value); /* { dg-warning "double-'free' of 'ptr.value'" } */
    }
}

void test_13 (void)
{
  boxed_ptr p = boxed_malloc (1024);
  boxed_ptr q = boxed_malloc (1024);

  foo ();
  if (!q.value)
    {
      boxed_free (q);
      return; /* { dg-warning "leak of 'p.value'" } */ 
    }
  bar ();
  boxed_free (q);
  boxed_free (p);
}

void test_14 (void)
{
  boxed_ptr p, q;
  p = boxed_malloc (1024);
  if (!p.value)
    return;

  q = boxed_malloc (1024);
  if (!q.value)
    {
      boxed_free (p);
      boxed_free (q);
      /* oops: missing "return".  */
    }
  bar ();
  boxed_free (q); /* Although this looks like a double-'free' of q,
	       it's known to be NULL for the case where free is
	       called twice on it.  */
  free (p.value); /* { dg-warning "double-'free' of 'p.value'" } */
}

void test_15 (void)
{
  boxed_ptr p, q;
  p.value = NULL;
  q.value = NULL;

  p = boxed_malloc (1024);
  if (!p.value)
    goto fail;

  foo ();

  q = boxed_malloc (1024);
  if (!q.value)
    goto fail;

  bar ();

 fail:
  boxed_free (q);
  boxed_free (p);
}

void test_16 (void)
{
  boxed_ptr p, q; /* { dg-message "region created on stack here" } */

  p = boxed_malloc (1024);
  if (!p.value)
    goto fail;

  foo ();

  q = boxed_malloc (1024);
  if (!q.value)
    goto fail;

  bar ();

 fail:
  boxed_free (q); /* { dg-warning "use of uninitialized value 'q'" } */
  boxed_free (p);
}

void test_17 (void)
{
  boxed_ptr ptr = boxed_malloc (1024);
} /* { dg-warning "leak of 'ptr.value'" } */ 

void test_18 (void)
{
  boxed_ptr ptr = boxed_malloc (64);
  ptr = boxed_null; /* { dg-warning "leak of 'ptr.value'" } */ 
}

void test_18a (void)
{
  boxed_ptr ptr = boxed_malloc (64);
  ptr.value = NULL; /* { dg-warning "leak of 'ptr.value'" } */ 
}

void test_19 (void)
{
  boxed_ptr ptr = boxed_malloc (64);
  free (ptr.value);
  ptr.value = NULL;
  free (ptr.value);
}

boxed_ptr global_ptr_20;

void test_20 (void)
{
  global_ptr_20 = boxed_malloc (1024);
}

int *test_21 (int i)
{
  boxed_ptr ptr = boxed_malloc (sizeof (int));
  if (!ptr.value)
    abort ();
  *(int *)ptr.value = i;
  return ptr.value;
}

boxed_ptr test_21a (int i)
{
  boxed_ptr ptr = boxed_malloc (sizeof (int));
  if (!ptr.value)
    abort ();
  *(int *)ptr.value = i;
  return ptr;
}

void test_22 (void)
{
  boxed_ptr ptr = boxed_malloc (1024);

  int i;
  for (i = 5; i < 10; i++)
    foo ();

  free (ptr.value);
  free (ptr.value); /* { dg-warning "double-'free' of 'ptr.value'" } */ 
}

int test_24 (void)
{
  boxed_ptr ptr;
  ptr.value = __builtin_alloca (sizeof (int)); /* { dg-message "region created on stack here" } */
  free (ptr.value); /* { dg-warning "'free' of 'ptr.value' which points to memory on the stack \\\[CWE-590\\\]" } */
}

int test_25 (void)
{
  char tmp[100]; /* { dg-message "region created on stack here" } */
  boxed_ptr p;
  p.value = tmp;
  free (p.value); /* { dg-warning "'free' of '&tmp' which points to memory on the stack \\\[CWE-590\\\]" } */
}

char global_buffer[100]; /* { dg-message "region created here" } */

int test_26 (void)
{
  boxed_ptr p;
  p.value = global_buffer;
  free (p.value); /* { dg-warning "'free' of '&global_buffer' which points to memory not on the heap \\\[CWE-590\\\]" } */
}

struct coord {
  float x;
  float y;
};

boxed_ptr test_27 (void)
{
  boxed_ptr p = boxed_malloc (sizeof (struct coord));
  ((struct coord *)p.value)->x = 0.f;  /* { dg-warning "dereference of possibly-NULL 'p.value' \\\[CWE-690\\\]" } */

  /* Only the first such usage should be reported: */
  ((struct coord *)p.value)->y = 0.f;

  return p;
}

struct link
{
  boxed_ptr m_ptr;
};

boxed_ptr test_29 (void)
{
  boxed_ptr res = boxed_malloc (sizeof (struct link));
  if (!res.value)
    return boxed_null;
  ((struct link *)res.value)->m_ptr = boxed_malloc (sizeof (struct link));
  return res;
}

void test_31 (void)
{
  struct link tmp;
  boxed_ptr ptr = boxed_malloc (sizeof (struct link));
  tmp.m_ptr = ptr;
} /* { dg-warning "leak" } */ 

void test_32 (void)
{
  boxed_ptr ptr = boxed_malloc (1024);
  could_free (ptr.value);
} /* { dg-bogus "leak" } */

void test_33 (void)
{
  boxed_ptr ptr = boxed_malloc (1024);
  cant_free (ptr.value);
} /* { dg-warning "leak of 'ptr.value'" } */ 

void test_34 (void)
{
  float *q;
  boxed_ptr p = boxed_malloc (sizeof (struct coord));
  if (!p.value)
    return;
  ((struct coord *)p.value)->x = 0.0f;
  q = &((struct coord *)p.value)->x;
  boxed_free (p);
  *q = 1.0f; /* { dg-warning "use after 'free' of 'q'" } */
};

int test_35 (void)
{
  boxed_ptr ptr = boxed_malloc(4096);
  if (!ptr.value)
    return -1;
  __builtin_memset(ptr.value, 0, 4096);
  boxed_free(ptr);
  return 0;
}

void test_36 (void)
{
  boxed_ptr ptr = boxed_malloc(4096);
  if (!ptr.value)
    return;
  __builtin_memset(ptr.value, 0, 4096);
  boxed_free(ptr);
}

boxed_ptr test_37a (void)
{
  boxed_ptr ptr = boxed_malloc(4096);
  __builtin_memset(ptr.value, 0, 4096); /* { dg-warning "use of possibly-NULL 'ptr.value' where non-null expected \\\[CWE-690\\\]" } */
  return ptr;
}

int test_37b (void)
{
  boxed_ptr p = boxed_malloc(4096);
  boxed_ptr q = boxed_malloc(4096);
  if (p.value) {
    __builtin_memset(p.value, 0, 4096); /* Not a bug: checked */
  } else {
    __builtin_memset(q.value, 0, 4096); /* { dg-warning "use of possibly-NULL 'q.value' where non-null expected \\\[CWE-690\\\]" } */
  }
  boxed_free(p);
  boxed_free(q);
  return 0;
}

extern void might_use_ptr (void *ptr);

void test_38(int i)
{
  boxed_ptr p;

  p = boxed_malloc(1024);
  if (p.value) {
    boxed_free(p);
    might_use_ptr(p.value); /* { dg-warning "use after 'free' of 'p.value'" "" { xfail *-*-* } } */
    // TODO: xfail
  }
}

boxed_ptr
test_39 (int i)
{
  boxed_ptr p = boxed_malloc(sizeof(int*));
  *(int *)p.value = i; /* { dg-warning "dereference of possibly-NULL 'p.value' \\\[CWE-690\\\]" } */
  return p;
}

boxed_ptr
test_41 (int flag)
{
  boxed_ptr buffer;

  if (flag) {
    buffer = boxed_malloc(4096);
  } else {
    buffer = boxed_null;
  }

  ((char *)buffer.value)[0] = 'a'; /* { dg-warning "dereference of possibly-NULL 'buffer.value' \\\[CWE-690\\\]" "possibly-NULL" } */
  /* { dg-warning "dereference of NULL" "NULL" { target *-*-* } .-1 } */

  return buffer;
}

extern void might_take_ownership (boxed_ptr ptr);

void test_45 (void)
{
  boxed_ptr p = boxed_malloc (1024);
  might_take_ownership (p);
}

/* Free of function, and of label within function.  */

void test_50a (void)
{
}

void test_50b (void)
{
  boxed_ptr ptr;
  ptr.value = test_50a;
  free (ptr.value); /* { dg-warning "'free' of '&test_50a' which points to memory not on the heap \\\[CWE-590\\\]" } */
}

void test_50c (void)
{
 my_label:
  boxed_ptr ptr;
  ptr.value = &&my_label;
  free (ptr.value); /* { dg-warning "'free' of '&my_label' which points to memory not on the heap \\\[CWE-590\\\]" } */
}

/* { dg-prune-output "\\\[-Wfree-nonheap-object" } */
