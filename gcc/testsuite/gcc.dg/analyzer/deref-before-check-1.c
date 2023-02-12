#define NULL ((void *)0)

int test_from_pr77432 (int *a)
{
    int b = *a; /* { dg-message "pointer 'a' is dereferenced here" } */
    if (a) /* { dg-warning "check of 'a' for NULL after already dereferencing it \\\[-Wanalyzer-deref-before-check\\\]" "warning" } */
         /* { dg-message "pointer 'a' is checked for NULL here but it was already dereferenced at \\(1\\)" "final event" { target *-*-* } .-1 } */
         return b;
    return 0;
}

int test_1a (int *p, int x)
{
  *p = x; /* { dg-message "pointer 'p' is dereferenced here" } */

  if (p) /* { dg-warning "check of 'p' for NULL after already dereferencing it \\\[-Wanalyzer-deref-before-check\\\]" "warning" } */
    /* { dg-message "pointer 'p' is checked for NULL here but it was already dereferenced at \\(1\\)" "final event" { target *-*-* } .-1 } */
    return 1;
  else
    return 0;
}

int test_1b (int *p, int *q)
{
  *q = *p; /* { dg-message "8: pointer 'p' is dereferenced here" } */

  if (p) /* { dg-warning "check of 'p' for NULL after already dereferencing it \\\[-Wanalyzer-deref-before-check\\\]" "warning" } */
    /* { dg-message "pointer 'p' is checked for NULL here but it was already dereferenced at \\(1\\)" "final event" { target *-*-* } .-1 } */
    return 1;
  else
    return 0;
}

struct s2
{
  int x;
  int y;
};

int test_2a (struct s2 *p)
{
  int sum = p->x + p->y; /* { dg-message "pointer 'p' is dereferenced here" } */
  if (!p) /* { dg-warning "check of 'p' for NULL after already dereferencing it" } */
    __builtin_abort ();
  return sum;
}

int test_2b (struct s2 *p)
{
  if (!p)
    __builtin_abort ();
  int sum = p->x + p->y;
  return sum;
}

struct s3
{
  int flag;
};

extern void err (const char *);

void test_3 (struct s3 *p)
{
  if (p->flag) /* { dg-message "pointer 'p' is dereferenced here" } */
    err ("p->flag");
  if (!p) /* { dg-warning "check of 'p' for NULL after already dereferencing it" } */
    err ("p was NULL");
}

struct s4
{
  struct s4 *m_next;
  int m_val;
};

int test_4 (struct s4 *p)
{
  if (p->m_next->m_val > 0) /* { dg-message "pointer '\\*p.m_next' is dereferenced here" } */
    return -1;
  if (!p->m_next) /* { dg-warning "check of '\\*p.m_next' for NULL after already dereferencing it" } */
    return -2;
  return p->m_next->m_val;
}

struct s5
{
  const char *str;
  int val;
};

int test_5 (struct s5 *p)
{
  __builtin_printf ("%s: %i\n", p->str, p->val); /* { dg-message "pointer 'p' is dereferenced here" } */
  if (p != NULL) /* { dg-warning "check of 'p' for NULL after already dereferencing it" } */
    return p->val;
  return -1;
}

static int  __attribute__((noinline))
__analyzer_check_ptr (int *p)
{
  if (p)
    return *p;
  else
    return 42;
}

int test_calling_check_ptr_after_deref_1 (int *q)
{
  int v = *q; /* { dg-bogus "dereferenced here" } */
  v += __analyzer_check_ptr (q);
  return v;
}

int test_calling_check_ptr_after_deref_2 (int *q)
{
  int v = *q; /* { dg-bogus "dereferenced here" } */
  v += __analyzer_check_ptr (q);
  *q = 17;
  return v;
}

int test_calling_check_ptr_after_deref_3 (int *q)
{
  int v = *q; /* { dg-message "pointer 'q' is dereferenced here" } */
  v += __analyzer_check_ptr (q);
  if (q) /* { dg-warning "check of 'q' for NULL after already dereferencing it" } */
    *q = 17;
  return v;
}

static int  __attribute__((noinline))
__analyzer_deref_ptr (int *p)
{
  return *p;
}

int test_calling_check_ptr_after_calling_deref_1 (int *q)
{
  int v = __analyzer_deref_ptr (q);
  v += __analyzer_check_ptr (q);
  return v;
}

int test_calling_check_ptr_after_calling_deref_2 (int *q)
{
  int v = __analyzer_deref_ptr (q);
  v += __analyzer_check_ptr (q);
  *q = 17;
  return v;
}

int test_calling_check_ptr_after_calling_deref_3 (int *q)
{
  int v =  __analyzer_deref_ptr (q);
  v += __analyzer_check_ptr (q);
  if (q)
    *q = 17;
  return v;
}

int test_checking_ptr_after_calling_deref (int *q)
{
  int v = __analyzer_deref_ptr (q);
  if (q)
    return 0;
  return v;
}

extern void foo ();
extern void bar ();
extern void baz ();

int test_cfg_diamond_1 (int *p, int flag)
{
  int x;
  x = *p; /* { dg-message "pointer 'p' is dereferenced here" } */
  if (flag)
    foo ();
  else
    bar ();
  if (p) /* { dg-warning "check of 'p' for NULL after already dereferencing it" } */
    {
      baz ();
    }
  return x;
}

int test_cfg_diamond_2 (int *p, int flag)
{
  int x = 0;
  if (flag)
    foo ();
  else
    {
      x = *p;
      bar ();
    }
  if (p) /* { dg-bogus "check of 'p' for NULL after already dereferencing it" } */
    {
      baz ();
    }
  return x;
}
