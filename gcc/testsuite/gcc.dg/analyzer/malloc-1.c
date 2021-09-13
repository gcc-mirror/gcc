/* { dg-require-effective-target alloca } */

#include <stdlib.h>

extern int foo (void);
extern int bar (void);
extern void could_free (void *);
extern void cant_free (const void *); /* since it's a const void *.  */

void test_1 (void)
{
  void *ptr = malloc (1024);
  free (ptr);
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}

void test_2 (void *ptr)
{
  free (ptr);
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}

void test_2a (void *ptr)
{
  __builtin_free (ptr);
  __builtin_free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}

int *test_3 (void)
{
  int *ptr = (int *)malloc (sizeof (int));
  *ptr = 42; /* { dg-warning "dereference of possibly-NULL 'ptr' \\\[CWE-690\\\]" } */
  return ptr;
}

int *test_3a (void)
{
  int *ptr = (int *)__builtin_malloc (sizeof (int));
  *ptr = 42; /* { dg-warning "dereference of possibly-NULL 'ptr' \\\[CWE-690\\\]" } */
  return ptr;
}

int *test_4 (void)
{
  int *ptr = (int *)malloc (sizeof (int));
  if (ptr)
    *ptr = 42;
  else
    *ptr = 43; /* { dg-warning "dereference of NULL 'ptr' \\\[CWE-476\\\]" } */
  return ptr;
}

int test_5 (int *ptr)
{
  free (ptr);
  return *ptr; /* { dg-warning "use after 'free' of 'ptr'" } */
}

void test_6 (void *ptr)
{
  void *q;
  q = ptr;
  free (ptr);
  free (q); /* { dg-warning "double-'free' of 'q'" } */
  /* The above case requires us to handle equivalence classes in
     state transitions.  */
}

void test_7 (void)
{
  void *ptr = malloc(4096);
  if (!ptr)
    return;
  __builtin_memset(ptr, 0, 4096);
  free(ptr);
}

void *test_8 (void)
{
  void *ptr = malloc(4096);
  if (!ptr)
    return NULL;
  __builtin_memset(ptr, 0, 4096);
  return ptr;
  /* This needs phi nodes to affect equivalence classes, or we get a false report
     of a leak.  */
}

void test_9 (void)
{
  void *ptr = malloc (1024);

  int i;
  for (i = 0; i < 1024; i++)
    free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}

void test_10 (void)
{
  void *ptr = malloc (1024);

  int i;
  for (i = 0; i < 1024; i++)
    foo ();

  free (ptr);
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */ 
}

void test_11 (void)
{
  void *ptr = malloc (1024);

  while (foo ())
    bar ();

  free (ptr);
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}

void test_12 (void)
{
  void *ptr = malloc (1024);

  while (1)
    {
      free (ptr);
      free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
    }
}

void test_13 (void)
{
  void *p = malloc (1024); /* { dg-message "allocated here" } */
  void *q = malloc (1024);

  foo ();
  if (!q)
    {
      free (q);
      return; /* { dg-warning "leak of 'p'" } */ 
    }
  bar ();
  free (q);
  free (p);
}

void test_14 (void)
{
  void *p, *q;
  p = malloc (1024);
  if (!p)
    return;

  q = malloc (1024);
  if (!q)
    {
      free (p);
      free (q);
      /* oops: missing "return".  */
    }
  bar ();
  free (q); /* Although this looks like a double-'free' of q,
	       it's known to be NULL for the case where free is
	       called twice on it.  */
  free (p); /* { dg-warning "double-'free' of 'p'" } */
}

void test_15 (void)
{
  void *p = NULL, *q = NULL;

  p = malloc (1024);
  if (!p)
    goto fail;

  foo ();

  q = malloc (1024);
  if (!q)
    goto fail;

  bar ();

 fail:
  free (q);
  free (p);
}

void test_16 (void)
{
  void *p, *q;

  p = malloc (1024);
  if (!p)
    goto fail;

  foo ();

  q = malloc (1024);
  if (!q)
    goto fail;

  bar ();

 fail:
  free (q); /* { dg-warning "use of uninitialized value 'q'" } */
  free (p);
}

void test_17 (void)
{
  void *ptr = malloc (1024); /* { dg-message "allocated here" } */
} /* { dg-warning "leak of 'ptr'" } */ 

void test_18 (void)
{
  void *ptr = malloc (64); /* { dg-message "allocated here" } */
  ptr = NULL; /* { dg-warning "leak of 'ptr'" } */ 
}

void test_19 (void)
{
  void *ptr = malloc (64);
  free (ptr);
  ptr = NULL;
  free (ptr);
}

void *global_ptr_20;

void test_20 (void)
{
  global_ptr_20 = malloc (1024);
}

int *test_21 (int i)
{
  int *ptr = malloc (sizeof (int));
  if (!ptr)
    abort ();
  *ptr = i;
  return ptr;
}

void test_22 (void)
{
  void *ptr = malloc (1024);

  int i;
  for (i = 5; i < 10; i++)
    foo ();

  free (ptr);
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */ 
}

int *test_23 (int n)
{
  int *ptr = (int *)calloc (n, sizeof (int));
  ptr[0] = 42; /* { dg-warning "dereference of possibly-NULL 'ptr' \\\[CWE-690\\\]" } */
  return ptr;
}

int *test_23a (int n)
{
  int *ptr = (int *)__builtin_calloc (n, sizeof (int));
  ptr[0] = 42; /* { dg-warning "dereference of possibly-NULL 'ptr' \\\[CWE-690\\\]" } */
  return ptr;
}

int test_24 (void)
{
  void *ptr = __builtin_alloca (sizeof (int)); /* { dg-message "memory is allocated on the stack here" } */
  free (ptr); /* { dg-warning "'free' of memory allocated on the stack by 'alloca' \\('ptr'\\) will corrupt the heap \\\[CWE-590\\\]" } */
}

int test_25 (void)
{
  char tmp[100];
  void *p = tmp; /* { dg-message "pointer is from here" } */
  free (p); /* { dg-warning "'free' of 'p' which points to memory not on the heap \\\[CWE-590\\\]" } */
  /* TODO: more precise messages here.  */
}

char global_buffer[100];

int test_26 (void)
{
  void *p = global_buffer; /* { dg-message "pointer is from here" } */
  free (p); /* { dg-warning "'free' of 'p' which points to memory not on the heap \\\[CWE-590\\\]" } */
  /* TODO: more precise messages here.  */
}

struct coord {
  float x;
  float y;
};

struct coord *test_27 (void)
{
  struct coord *p = (struct coord *) malloc (sizeof (struct coord)); /* { dg-message "this call could return NULL" } */
  p->x = 0.f;  /* { dg-warning "dereference of possibly-NULL 'p' \\\[CWE-690\\\]" } */

  /* Only the first such usage should be reported: */
  p->y = 0.f;

  return p;
}

struct coord *test_28 (void)
{
  struct coord *p = NULL;
  p->x = 0.f; /* { dg-warning "dereference of NULL 'p' \\\[CWE-476\\\]" } */

  /* Only the first such usage should be reported: */
  p->y = 0.f;

  return p;
}

struct link
{
  struct link *m_ptr;
};

struct link *test_29 (void)
{
  struct link *res = (struct link *)malloc (sizeof (struct link));
  if (!res)
    return NULL;
  res->m_ptr = (struct link *)malloc (sizeof (struct link));
  return res;
}

struct link *test_29a (void)
{
  struct link *res = (struct link *)malloc (sizeof (struct link));
  if (!res)
    return NULL;
  res->m_ptr = (struct link *)malloc (sizeof (struct link));
  if (!res->m_ptr)
    {
      free (res);
      return NULL;
    }
  res->m_ptr->m_ptr = (struct link *)malloc (sizeof (struct link));
  return res;
}

/* Without consolidation by EC, this one shows two leaks:
     warning: leak of '<unknown>'
     warning: leak of 'tmp.m_ptr' 
   We should only show the latter (favoring the most user-readable
   expression in the equivalence class).  */
void test_30 (void)
{
  struct link tmp;
  tmp.m_ptr = (struct link *)malloc (sizeof (struct link)); /* { dg-message "allocated here" } */
} /* { dg-warning "leak of 'tmp.m_ptr'" } */ 
/* { dg-bogus "leak of '<unknown>'" "leak of unknown" { target *-*-* } .-1 } */

void test_31 (void)
{
  struct link tmp;
  void *ptr = malloc (sizeof (struct link)); /* { dg-message "allocated here" } */
  tmp.m_ptr = (struct link *)ptr;
} /* { dg-warning "leak of 'ptr'" } */ 
/* { dg-bogus "leak of 'tmp.m_ptr'" "" { target *-*-* } .-1 } */

void test_32 (void)
{
  void *ptr = malloc (1024);
  could_free (ptr);
} /* { dg-bogus "leak" } */

void test_33 (void)
{
  void *ptr = malloc (1024); /* { dg-message "allocated here" } */
  cant_free (ptr);
} /* { dg-warning "leak of 'ptr'" } */ 

void test_34 (void)
{
  float *q;
  struct coord *p = malloc (sizeof (struct coord));
  if (!p)
    return;
  p->x = 0.0f;
  q = &p->x;
  free (p);
  *q = 1.0f; /* { dg-warning "use after 'free' of 'q'" } */
};

int test_35 (void)
{
  void *ptr = malloc(4096);
  if (!ptr)
    return -1;
  __builtin_memset(ptr, 0, 4096);
  free(ptr);
  return 0;
}

void test_36 (void)
{
  void *ptr = malloc(4096);
  if (!ptr)
    return;
  __builtin_memset(ptr, 0, 4096);
  free(ptr);
}

void *test_37a (void)
{
  void *ptr = malloc(4096); /* { dg-message "this call could return NULL" } */
  __builtin_memset(ptr, 0, 4096); /* { dg-warning "use of possibly-NULL 'ptr' where non-null expected \\\[CWE-690\\\]" } */
  return ptr;
}

int test_37b (void)
{
  void *p = malloc(4096);
  void *q = malloc(4096); /* { dg-message "this call could return NULL" } */
  if (p) {
    __builtin_memset(p, 0, 4096); /* Not a bug: checked */
  } else {
    __builtin_memset(q, 0, 4096); /* { dg-warning "use of possibly-NULL 'q' where non-null expected \\\[CWE-690\\\]" } */
  }
  free(p);
  free(q);
  return 0;
}

extern void might_use_ptr (void *ptr);

void test_38(int i)
{
  void *p;

  p = malloc(1024);
  if (p) {
    free(p);
    might_use_ptr(p); /* { dg-warning "use after 'free' of 'p'" "" { xfail *-*-* } } */
    // TODO: xfail
  }
}

int *
test_39 (int i)
{
  int *p = (int*)malloc(sizeof(int*)); /* { dg-message "this call could return NULL" } */
  *p = i; /* { dg-warning "dereference of possibly-NULL 'p' \\\[CWE-690\\\]" } */
  return p;
}

int *
test_40 (int i)
{
  int *p = (int*)malloc(sizeof(int*));
  i = *p; /* { dg-warning "dereference of possibly-NULL 'p' \\\[CWE-690\\\]" "possibly-null" } */
  /* { dg-warning "use of uninitialized value '\\*p'" "uninit" { target *-*-*} .-1 } */
  return p;
}

char *
test_41 (int flag)
{
  char *buffer;

  if (flag) {
    buffer = (char*)malloc(4096);
  } else {
    buffer = NULL;
  }

  buffer[0] = 'a'; /* { dg-warning "dereference of possibly-NULL 'buffer' \\\[CWE-690\\\]" "possibly-NULL" } */
  /* { dg-warning "dereference of NULL 'buffer' \\\[CWE-476\\\]" "NULL" { target *-*-* } .-1 } */

  return buffer;
}

void test_42a (void)
{
  void *p = malloc (1024); /* { dg-message "allocated here" } */
  free (p + 64); /* this could well corrupt the heap.  */
  /* TODO: ^^^ we should warn about this.  */
} /* { dg-warning "leak of 'p'" } */
/* TODO: presumably we should complain about the bogus free, but then
   maybe not complain about the leak.  */
// CWE-761: Free of Pointer not at Start of Buffer

void test_42b (void)
{
  void *p = malloc (1024); /* { dg-message "allocated here" } */
  free (p - 64); /* this could well corrupt the heap.  */
  /* TODO: ^^^ we should warn about this.  */
} /* { dg-warning "leak of 'p'" } */
/* TODO: presumably we should complain about the bogus free, but then
   maybe not complain about the leak.  */
// CWE-761: Free of Pointer not at Start of Buffer

void test_42c (void)
{
  void *p = malloc (1024);
  void *q = p + 64;
  free (q - 64); /* this is probably OK.  */
} /* { dg-bogus "leak of 'p'" } */

void *
test_42d (void)
{
  void *p = malloc (1024);
  void *q = p + 64;
  return q;
} /* { dg-bogus "leak of 'p'" } */

#if 0
void test_31 (void *p)
{
  void *q = realloc (p, 1024);
  free (p); /* FIXME: this is a double-'free'.  */
  free (q);
}

void test_32 (void)
{
  void *p = malloc (64);
  p = realloc (p, 1024); /* FIXME: this leaks if it fails.  */
  free (p);
}
#endif

struct link global_link;

void test_43 (void)
{
  global_link.m_ptr = malloc (sizeof (struct link)); /* { dg-message "allocated here" } */
  global_link.m_ptr = NULL; /* { dg-warning "leak of 'global_link.m_ptr'" } */
}

struct link *global_ptr;

void test_44 (void)
{
  global_ptr = malloc (sizeof (struct link));
  if (!global_ptr)
    return;
  global_ptr->m_ptr = malloc (sizeof (struct link)); /* { dg-message "allocated here" } */
  free (global_ptr); /* { dg-warning "leak of '<unknown>'" } */
  /* TODO: should be more precise than just '<unknown>'.  */
}

extern void might_take_ownership (void *ptr);

void test_45 (void)
{
  void *p = malloc (1024);
  might_take_ownership (p);
}

void test_46 (void)
{
  struct link *p = (struct link *)malloc (sizeof (struct link));
  if (!p)
    return;
  struct link *q = (struct link *)malloc (sizeof (struct link));
  p->m_ptr = q;
  might_take_ownership (p);
}

extern int maybe_alloc (char **);

int test_47 (void)
{
  char *p = ((void *)0);
  int p_size = 0;

  p = malloc (16);
  if (p) {
    free (p);
  } else {
    int retval = maybe_alloc (&p); /* this might write to "p".  */
    if (retval)
      return (retval);
    p_size = __builtin_strlen(p); /* { dg-bogus "non-null expected" } */
    free (p);
  }
  return p_size;
}

void test_48 (void)
{
  int *p = NULL; /* { dg-message "'p' is NULL" } */
  *p = 1; /* { dg-warning "dereference of NULL 'p' \\\[CWE-476\\\]" } */
}

/* As test_48, but where the assignment of NULL is not at the start of a BB.  */

int test_49 (int i)
{
  int *p;
  int x;

  x = i * 2;
  p = NULL; /* { dg-message "'p' is NULL" } */
  *p = 1; /* { dg-warning "dereference of NULL 'p' \\\[CWE-476\\\]" } */
  return x;
}

/* { dg-prune-output "\\\[-Wfree-nonheap-object" } */
