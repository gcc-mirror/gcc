#include <stdlib.h>

struct st
{
  void *m_f;
};

struct node
{
  struct node *m_next;
};

extern void unknown_fn (void *);
extern void const_unknown_fn (const void *);

void
test_1 (struct st *p, struct st *q)
{
  p->m_f = malloc (1024);
  q->m_f = NULL; /* { dg-bogus "leak" } */
  free (p->m_f);
}

void
test_2 (void)
{
  struct st s;
  s.m_f = malloc (1024);
  unknown_fn (&s);
  free (s.m_f);
}

void
test_3 (void)
{
  struct st s;
  s.m_f = malloc (1024);
  const_unknown_fn (&s);
  free (s.m_f);
}

void
test_4 (void)
{
  struct st s;
  s.m_f = malloc (1024);
  unknown_fn (&s);
} /* { dg-bogus "leak" } */

void
test_5 (void)
{
  struct st s;
  s.m_f = malloc (1024);
  /* s is const, but the pointer could still be freed; hence not a leak.  */
  const_unknown_fn (&s);
} /* { dg-bogus "leak" } */

void
test_6 (void)
{
  struct st s;
  s.m_f = malloc (1024);
} /* { dg-warning "leak" } */

struct st
test_7 (void)
{
  struct st s;
  s.m_f = malloc (1024);
  return s;
} /* { dg-bogus "leak" } */

struct node *
test_8 (void)
{
  struct node *n1 = malloc (sizeof (struct node));
  if (!n1)
    return NULL;
  n1->m_next = malloc (sizeof (struct node));
  return n1;
}

void
test_9 (void)
{
  struct node *n1 = malloc (sizeof (struct node));
  if (!n1)
    return;
  n1->m_next = malloc (sizeof (struct node));
  /* Could free n1 and n1->m_next.  */
  unknown_fn (n1);
}

void
test_10 (void)
{
  struct node *n1 = malloc (sizeof (struct node));
  if (!n1)
    return;
  n1->m_next = malloc (sizeof (struct node));
  /* Could free n1->m_next, but not n1.  */
  const_unknown_fn (n1); /* { dg-warning "leak of 'n1'" } */
}

void
test_11 (void)
{
  struct node *n1 = malloc (sizeof (struct node));
  if (!n1)
    return;
  n1->m_next = malloc (sizeof (struct node));
  /* Could free n1->m_next, but not n1.  */
  unknown_fn (n1->m_next); /* { dg-warning "leak of 'n1'" } */
}

void
test_12a (void)
{
  int *ip = malloc (sizeof (int));
  *ip = 42; /* { dg-warning "dereference of possibly-NULL 'ip'" } */
  free (ip);
}

void
test_12b (void)
{
  int *ip = malloc (sizeof (int));
  unknown_fn (ip);
  /* Might not be a null-deref, as unknown_fn could abort on NULL.  */
  *ip = 42;
  free (ip);
}

void
test_12c (void)
{
  int *ip = malloc (sizeof (int));
  /* Might not be a null-deref, as const_unknown_fn could abort on NULL.
     Right now we don't have a great way of handling this.  */
  const_unknown_fn (ip);
  *ip = 42; /* { dg-bogus "dereference of possibly-NULL 'ip'" "" { xfail *-*-* } } */
  free (ip);
}
