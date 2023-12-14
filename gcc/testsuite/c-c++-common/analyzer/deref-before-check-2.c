#include <stdio.h>

struct st
{
  char *str;
  int i;
};

int test_1 (struct st *p)
{
  fprintf (stderr, "str: %s\n", p->str); /* { dg-message "pointer 'p' is dereferenced here" } */
  if (!p) /* { dg-warning "check of 'p' for NULL after already dereferencing it" } */
    return -1;
  return p->i;  
}

int test_2 (int flag_a, int flag_b, struct st *p)
{
  if (flag_a)
    {
      int j = p->i; /* { dg-message "pointer 'p' is dereferenced here" } */
      if (flag_b && p) /* { dg-warning "check of 'p' for NULL after already dereferencing it" } */
	return 1;
      return j;
    }
  return 2;
}

int test_3 (struct st *a, struct st *b)
{
  if (!a)
    return b->i;
  if (!b)
    return a->i;
  return 0;
}

int test_4 (struct st *p)
{
  int *q = &p->i;
  if (!p)
    return -1;
  return *q;  
}

void test_check_after_strlen (const char *str)
{
  size_t len_a = __builtin_strlen (str); /* { dg-message "pointer 'str' is dereferenced here" } */
  size_t len_b = str ? __builtin_strlen (str) : 0; /* { dg-warning "check of 'str' for NULL after already dereferencing it" } */
}

void test_6 (struct st *a, struct st *b)
{
  int diff = a->i - b->i; /* { dg-message "pointer 'b' is dereferenced here" } */

  /* ... */

  if (b) /* { dg-warning "check of 'b' for NULL after already dereferencing it" } */
    fprintf (stderr, "str: %s\n", b->str);
}

void test_check_after_strcmp (const char *s1, const char *s2)
{
  if (!__builtin_strcmp (s1, s2)) /* { dg-message "pointer 's1' is dereferenced here" } */
    return;

  /* ... */

  if (s1) /* { dg-warning "check of 's1' for NULL after already dereferencing it" } */
    return;
}

void test_more_than_one_deref (struct st *p)
{
  char *str = p->str; /* { dg-message "pointer 'p' is dereferenced here" } */
  int i = p->i;
  
  /* ... */

  if (p) /* { dg-warning "check of 'p' for NULL after already dereferencing it" } */
    return;

  /* ... */
}

void test_deref_under_another_name (struct st *p)
{
  struct st *q = p;
  int i = q->i; /* { dg-message "pointer 'p' is dereferenced here" } */

  /* ... */

  if (p) /* { dg-warning "check of 'p' for NULL after already dereferencing it" } */
    return;

  /* ... */
}

void test_check_after_memcpy_src (struct st *dst, struct st *src)
{
  __builtin_memcpy (dst, src, sizeof (struct st)); /* { dg-message "pointer 'src' is dereferenced here" } */

  /* ... */

  if (!src) /* { dg-warning "check of 'src' for NULL after already dereferencing it" } */
    return;

  /* ... */
}

void test_check_after_memcpy_dst (struct st *dst, struct st *src)
{
  __builtin_memcpy (dst, src, sizeof (struct st)); /* { dg-message "pointer 'dst' is dereferenced here" } */

  /* ... */

  if (!dst) /* { dg-warning "check of 'dst' for NULL after already dereferencing it" } */
    return;

  /* ... */
}

void test_merger (int *p, int flag)
{
  int x = *p;
  if (flag)
    __builtin_free (p);
  if (!flag)
    __builtin_free (p); /* { dg-bogus "double-'free'" } */
}
