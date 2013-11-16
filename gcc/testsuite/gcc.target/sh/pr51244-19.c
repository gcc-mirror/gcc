/* Check that no unnecessary T bit stores are done before conditional
   branches.
   This case was extracted from the CSiBE set and contained the following
   sequence:
	mov.l	@(8,r4),r2
	mov.l	@(4,r4),r3
	cmp/gt	r2,r3
	movt	r2
.L3:
	tst	r2,r2
	bt/s	.L12
	mov	#-1,r0

	.....

	mov.l	@r4,r2
	tst	r2,r2
	bra	.L3
	movt	r2

   In this reduced code the movt insns were only present in the
   unwanted sequences.  Thus, if we see any movt insns, something is not
   working as expected.  This test requires -O2 because the T bit stores
   in question will be eliminated in additional insn split passes after
   reload.  */
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } } */
/* { dg-final { scan-assembler-not "movt" } } */

struct request
{
 unsigned long nr_sectors;
};

struct request_list
{
 int count;
};

struct request_queue
{
 struct request_list rq;
 volatile int nr_sectors;
 int max_queue_sectors;
 int can_throttle;
 unsigned long bounce_pfn;
};

typedef struct request_queue request_queue_t;

static inline int
blk_oversized_queue (request_queue_t* q)
{
  if (q->can_throttle)
    return q->nr_sectors > q->max_queue_sectors;
  return q->rq.count == 0;
}

struct request*
get_request (request_queue_t* q, int rw)
{
  struct request* rq = ((void*)0);
  struct request_list *rl = &q->rq;

  if (blk_oversized_queue (q))
    {
      if ((rw == 1) || (rw == 0))
	return ((void*)0);
      if (blk_oversized_queue (q))
	return ((void*)0);
    }

  return (void*)-100;
}
