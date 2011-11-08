/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O -fdump-tree-tmmark" } */

/* Test that `nontrxn' doesn't end up inside the transaction.  */

typedef struct node {
  int * val;
  struct node *next;
} node_t;

node_t *next;
int nontrxn1, nontrxn;

static int set_remove(int * val)
{
  int result;
  int * v;
  __transaction_relaxed {
    v = next->val;
    result = (v == val);
    if (result)
      result = 2;
  }
  return result;
}

void test(void *data)
{
  extern void bark(void);
  if (set_remove(0))
    bark();
  nontrxn = 99;			/* Should be outside transaction.  */
}

/* { dg-final { scan-tree-dump-times "_ITM_W.*nontrxn" 0 "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmmark" } } */
