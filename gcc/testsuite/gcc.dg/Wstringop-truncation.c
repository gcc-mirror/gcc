/* PR tree-optimization/84468 - Inconsistent -Wstringop-truncation warnings
   with -O2
   { dg-do compile }
   { dg-options "-O2 -Wstringop-truncation -ftrack-macro-expansion=0 -g" }  */

#define strncpy __builtin_strncpy

struct A
{
  char a[4];
};

void no_pred_succ_lit (struct A *p)
{
  /* The following is folded early on, before the strncpy statement
     has a basic block.  Verify that the case is handled gracefully
     (i.e., there's no assumption that the statement does have
     a basic block).  */
  strncpy (p->a, "1234", sizeof p->a - 1);    /* { dg-warning "\\\[-Wstringop-truncation" } */
}

/* Verify a strncpy call in a basic block with no predecessor or
   successor.  */
void no_pred_succ (struct A *p, const struct A *q)
{
  strncpy (p->a, q->a, sizeof p->a - 1);      /* { dg-warning "\\\[-Wstringop-truncation" } */
}


/* Verify a strncpy call in a basic block with no successor.  */
void no_succ (struct A *p, const struct A *q)
{
  if (q->a)
    strncpy (p->a, q->a, sizeof p->a - 1);    /* { dg-warning "\\\[-Wstringop-truncation" } */
}

/* Verify a strncpy call in a basic block with nul assignment in
   a successor block.  */
void succ (struct A *p, const struct A *q)
{
  /* Verify that the assignment suppresses the warning for the conditional
     strcnpy call.  The conditional should be folded to true since the
     address of an array can never be null (see bug 84470).  */
  if (q->a)
    strncpy (p->a, q->a, sizeof p->a - 1);    /* { dg-bogus "\\\[-Wstringop-truncation" } */

  p->a[sizeof p->a - 1] = 0;
}


void succ_2 (struct A *p, const struct A *q, int i)
{
  /* Same as above but with a conditional that cannot be eliminated.  */
  if (i < 0)
    strncpy (p->a, q->a, sizeof p->a - 1);    /* { dg-bogus "\\\[-Wstringop-truncation" } */

  p->a[sizeof p->a - 1] = 0;
}


/* Verify a strncpy call in a basic block with nul assignment in
   the next successor block.  */
int next_succ (struct A *p, const struct A *q, int i, int j)
{
  /* Same as above but with a nested conditionals with else clauses.  */
  if (i < 0)
    {
      if (j < 0)
	strncpy (p->a, q->a, sizeof p->a - 1);    /* { dg-bogus "\\\[-Wstringop-truncation" } */
    }
  else
    __builtin_strcpy (p->a, q->a);

  p->a[sizeof p->a - 1] = 0;
  return 0;
}


int next_succ_1 (struct A *p, const struct A *q, int i, int j)
{
  /* Same as above but with a nested conditionals with else clauses.  */
  if (i < 0)
    {
      if (j < 0)
	strncpy (p->a, q->a, sizeof p->a - 1);    /* { dg-bogus "\\\[-Wstringop-truncation" } */
      else
	strncpy (p->a, q->a, sizeof p->a - 2);    /* { dg-bogus "\\\[-Wstringop-truncation" } */
    }

  p->a[sizeof p->a - 2] = 0;
  return 1;
}


int next_succ_2 (struct A *p, const struct A *q, int i, int j)
{
  /* Same as above but with a nested conditionals with else clauses.  */
  if (i < 0)
    {
      if (j < 0)
	strncpy (p->a, q->a, sizeof p->a - 1);    /* { dg-bogus "\\\[-Wstringop-truncation" } */
      else
	strncpy (p->a, q->a, sizeof p->a - 2);    /* { dg-bogus "\\\[-Wstringop-truncation" } */
    }
  else
    __builtin_strcpy (p->a, q->a);

  p->a[sizeof p->a - 2] = 0;
  return 2;
}


void cond_succ_warn (struct A *p, const struct A *q, int i)
{
  /* Verify that a conditional assignment doesn't suppress the warning.  */
  strncpy (p->a, q->a, sizeof p->a - 1);      /* { dg-warning "\\\[-Wstringop-truncation" } */

  if (i < 0)
    p->a[sizeof p->a - 1] = 0;
}

void cond_succ_nowarn (struct A *p, const struct A *q)
{
  /* Verify that distinct but provably equivalent conditionals are
     recognized and don't trigger the warning.  */
  if (p != q)
    strncpy (p->a, q->a, sizeof p->a - 1);

  if (p->a != q->a)
    p->a[sizeof p->a - 1] = 0;
}
