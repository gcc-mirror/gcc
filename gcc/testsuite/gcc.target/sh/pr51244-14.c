/* This is a case extracted from CSiBE which would sometimes contain the
   following sequence:
	cmp/eq	r12,r13
	movt	r0
	xor	#1,r0
	extu.b	r0,r0
	movt	r3
	tst	r0,r0
	bf/s	.L35
   where the negated T bit store did not combine properly.  Since there are
   other movt insns we only check for the xor and the extu.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } } */
/* { dg-final { scan-assembler-not "xor|extu" } } */

typedef struct transaction_s transaction_t;

struct journal_head
{
  transaction_t * b_transaction;
  struct journal_head *b_cpnext, *b_cpprev;
};

struct transaction_s
{
  struct journal_head * t_checkpoint_list;
  transaction_t *t_cpnext, *t_cpprev;
};

struct journal_s
{
  transaction_t * j_checkpoint_transactions;
  unsigned long j_first, j_last;
};

typedef struct journal_s journal_t;

extern int __try_to_free_cp_buf (struct journal_head *jh);
extern int __cleanup_transaction (journal_t *journal, transaction_t *transaction);
extern void __flush_batch (void **bhs, int *batch_count);
extern void* jh2bh (void*);

static int
__flush_buffer (journal_t *journal, struct journal_head *jh,
		void **bhs, int *batch_count, int *drop_count)
{
  void *bh = jh2bh (jh);
  int ret = 0;
  if (bh)
    {
      bhs[*batch_count] = bh;
      (*batch_count)++;
      if (*batch_count == 64)
	  ret = 1;
    }
  else
    {
      int last_buffer = 0;
      if (jh->b_cpnext == jh)
	last_buffer = 1;
      if (__try_to_free_cp_buf (jh))
	{
	  (*drop_count)++;
	  ret = last_buffer;
	}
    }
  return ret;
}

int
log_do_checkpoint (journal_t *journal, int nblocks)
{
  transaction_t *transaction, *last_transaction, *next_transaction;
  int batch_count = 0;
  void *bhs[64];

repeat:
  transaction = journal->j_checkpoint_transactions;
  if (transaction == ((void *)0))
    return 0;
  last_transaction = transaction->t_cpprev;
  next_transaction = transaction;
  do
    {
      struct journal_head *jh, *last_jh, *next_jh;
      int drop_count = 0;
      int cleanup_ret, retry = 0;
      transaction = next_transaction;
      next_transaction = transaction->t_cpnext;
      jh = transaction->t_checkpoint_list;
      last_jh = jh->b_cpprev;
      next_jh = jh;
      do
	{
	  jh = next_jh;
	  next_jh = jh->b_cpnext;
	  retry = __flush_buffer(journal, jh, bhs, &batch_count, &drop_count);
	} while (jh != last_jh && !retry);

      if (retry)
	goto repeat;

      cleanup_ret = __cleanup_transaction(journal, transaction);
      goto repeat;
    } while (transaction != last_transaction);
}
