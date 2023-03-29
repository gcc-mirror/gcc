struct st1;

int foo (struct st1 *p);
int bar (struct st1 *p);

void test_1 (struct st1 *p)
{
  test_1 (p); /* { dg-warning "infinite recursion" } */
}

void test_2_if (struct st1 *p)
{
  if (foo (p))
    test_2_if (p); /* { dg-bogus "infinite recursion" } */
}

void test_2_switch (struct st1 *p)
{
  switch (foo (p))
    {
    case 0 ... 9:
      test_2_switch (p); /* { dg-bogus "infinite recursion" } */
      break;
    default:
      break;
    }
}

void test_2_if_compound (struct st1 *p)
{
  if ((foo (p) + bar (p)) >= 0)
    test_2_if_compound (p); /* { dg-bogus "infinite recursion" } */
}

void test_3 (struct st1 *p)
{
  foo (p);
  test_3 (p); /* { dg-warning "infinite recursion" } */
  /* The content of *p never affects control flow, so we should
     report this.  */
}

struct st2
{
  int i;
};

void test_4 (struct st2 *p)
{
  if (p->i > 0)
    test_4 (p); /* { dg-warning "infinite recursion" } */
}

void test_5 (struct st2 *p)
{
  if (p->i-- > 0)
    test_5 (p); /* { dg-bogus "infinite recursion" } */
}

/* Mixtures of heap allocation and recursion.  It's not clear what we
   should do for such cases, but make sure we don't ICE.  */

void test_6 (struct st2 *p)
{
  struct st2 *q = __builtin_malloc (p->i);
  if (!q)
    return;
  q->i = p->i;
  test_6 (q);
  __builtin_free (q);
}

void test_7 (struct st2 *p)
{
  struct st2 *q = __builtin_malloc (p->i);
  q->i = p->i; /* { dg-warning "dereference of possibly-NULL 'q'" } */
  test_7 (q);
  __builtin_free (q);
}

void test_switch_1 (int i)
{
  int j;
  switch (i)
    {
    case 0:
      j = 1066;
      break;
    case 1:
      j = 1776;
      break;
    default:
      j = 1492;
      break;
    }
  test_switch_1 (j);  /* { dg-warning "infinite recursion" "" { xfail *-*-* } } */
}

void test_switch_2 (int i)
{
  switch (i)
    {
    case 0:
      test_switch_2 (1066);
      break;
    case 1:
      test_switch_2 (1776);
      break;
    default:
      test_switch_2 (1492); /* { dg-warning "infinite recursion" "" { xfail *-*-* } } */
      break;
    }
}
