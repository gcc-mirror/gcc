/* Various correct and incorrect ways to walk a linked list, accumulating
   a value.  */

/* { dg-additional-options "-O0" } */

extern int maybe_useful_work ();

struct node
{
  struct node *next;
  int val;
};

/* Various "while" loops.  */

int correct_while_loop (struct node *n)
{
  int sum = 0;
  while (n)
    {
      sum += n->val;
      n = n->next;
    }
  return sum;
}

int while_loop_missing_next (struct node *n)
{
  int sum = 0;
  while (n) /* { dg-line "while_loop_missing_next_WHILE_LINE" } */
    {
      sum += n->val; /* { dg-line "while_loop_missing_next_LOOP_BODY" } */
      /* We're missing: "n = n->next;", so n does not change  */
    }
  return sum;

  /* { dg-warning "10: infinite loop" "" { target *-*-* } while_loop_missing_next_WHILE_LINE } */
  /* { dg-message "10: \\(2\\) when 'n' is non-NULL: always following 'true' branch\.\.\." "" { target *-*-* } while_loop_missing_next_WHILE_LINE } */
  /* { dg-message "\\(3\\) \.\.\.to here" "" { target *-*-* } while_loop_missing_next_LOOP_BODY } */
  /* { dg-message "\\(4\\) looping back\.\.\." "" { target *-*-* } while_loop_missing_next_LOOP_BODY } */
  /* { dg-message "\\(5\\) \.\.\.to here" "" { target *-*-* } while_loop_missing_next_WHILE_LINE } */
}

int while_loop_missing_next_with_work (struct node *n)
{
  int sum = 0;
  while (n) /* { dg-bogus "infinite loop" } */
    {
      sum += n->val; 
      /* We're missing: "n = n->next;", so n does not change  */
      /* But here we do something that could be doing useful work.  */
      maybe_useful_work ();
    }
  return sum;
}

/* BUG: missing: "sum += ", so sum does not change.  */

int while_loop_missing_add (struct node *n)
{
  int sum = 0;
  while (n) /* { dg-warning "infinite loop" } */
    n->val;
  return sum;
}

/* Various "for" loops.  */

int correct_for_loop (struct node *n)
{
  int sum = 0;
  for (struct node *iter = n; iter; iter = iter->next)
    sum += n->val;
  return sum;
}

int for_loop_missing_condition (struct node *n)
{
  int sum = 0;
  for (struct node *iter = n; ; iter = iter->next)
    sum += n->val; /* { dg-warning "infinite loop" } */
  return sum;
}

int for_loop_missing_next (struct node *n)
{
  int sum = 0;
  for (struct node *iter = n; iter; ) /* { dg-warning "infinite loop" } */
    sum += n->val;
  return sum;
}

/* BUG: missing: "sum += ", so sum does not change.
   Not an infinite loop though, as we do iterate to the
   end of the list.  */

int for_loop_missing_add (struct node *n)
{
  int sum = 0;
  for (struct node *iter = n; iter; iter = iter->next)
    n->val; /* { dg-bogus "infinite loop" } */
  return sum;
}

/* BUG: "iter->next" should be "iter = iter->next".  */

int for_loop_noop_next (struct node *n)
{
  int sum = 0;
  for (struct node *iter = n; iter; iter->next) /* { dg-line for_loop_noop_next_FOR_LINE } */
    sum += n->val; /* { dg-line for_loop_noop_next_LOOP_BODY } */
  return sum;

  /* { dg-warning "31: infinite loop" "" { target *-*-* } for_loop_noop_next_FOR_LINE } */
  /* { dg-message "31: \\(2\\) when 'iter' is non-NULL: always following 'true' branch\.\.\." "" { target *-*-* } for_loop_noop_next_FOR_LINE } */
  /* { dg-message "\\(3\\) \.\.\.to here" "" { target *-*-* } for_loop_noop_next_LOOP_BODY } */
  /* { dg-message "\\(4\\) looping back\.\.\." "" { target *-*-* } for_loop_noop_next_LOOP_BODY } */
  /* { dg-message "\\(5\\) \.\.\.to here" "" { target *-*-* } for_loop_noop_next_FOR_LINE } */
}

/* BUG: "iter = n->next" should be "iter = iter->next"
   and so it becomes an infinite loop at the 2nd element.  */

int for_loop_wrong_next (struct node *n)
{
  int sum = 0;
  for (struct node *iter = n; iter; iter = n->next) /* { dg-warning "infinite loop" "" { xfail *-*-* } } */
    /* TODO: xfail.  */
    sum += n->val; 
  return sum;
}
