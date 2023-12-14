struct st
{
  int a, b, c, d, e;
};

int
test_1 (int flag, struct st *p)
{
  struct st *q;
  int result = 0;
  if (flag)
    q = p;
  /* We should only warn about the first use of uninit for 'q':  */
  result += q->a; /* { dg-warning "use of uninitialized value 'q'" } */
  /* ...and not for these:  */
  result += q->b; /* { dg-bogus "use of uninitialized value 'q'" } */
  result += q->c; /* { dg-bogus "use of uninitialized value 'q'" } */
  result += q->d; /* { dg-bogus "use of uninitialized value 'q'" } */
  result += q->e; /* { dg-bogus "use of uninitialized value 'q'" } */
  return result;
}

int
test_2 (int flag, struct st *p, struct st *r)
{
  struct st *q;
  int result = 0;
  if (flag)
    q = p;
  /* We should only warn about the first use of uninit for 'q':  */
  if (q == r) /* { dg-warning "use of uninitialized value 'q'" } */
    result += 1;
  /* ...and not for these, after a conditional:  */
  result += q->b; /* { dg-bogus "use of uninitialized value 'q'" } */
  result += q->c; /* { dg-bogus "use of uninitialized value 'q'" } */
  result += q->d; /* { dg-bogus "use of uninitialized value 'q'" } */
  result += q->e; /* { dg-bogus "use of uninitialized value 'q'" } */
  return result;
}

int
test_3 (int flag, int val)
{
  int result = 0;
  int idx;
  if (flag)
    idx = val;
  switch (idx) /* { dg-warning "use of uninitialized value 'idx'" } */
    {
    case 0:
      result = 3;
      break;
    case 1:
      result = 4;
      break;
    default:
      result = 5;
      break;      
    }
  switch (idx) /* { dg-bogus "use of uninitialized value 'idx'" } */
    {
    case 0:
      result += 3;
      break;
    case 1:
      result += 4;
      break;
    default:
      result += 5;
      break;
    }
  return result;
}
