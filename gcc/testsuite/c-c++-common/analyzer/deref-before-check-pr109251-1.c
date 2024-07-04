/* Reduced from linux-5.10.162's kernel/sched/fair.c,
   with !CONFIG_FAIR_GROUP_SCHED.  */

#define NULL ((void*)0)

struct load_weight
{
  unsigned long weight;
  /* [...snip...] */
};

struct sched_entity
{
  struct load_weight load;
  /* [...snip...] */
  unsigned int on_rq;
  /* [...snip...] */
};

struct cfs_rq
{
  /* [...snip...] */
  unsigned int nr_running;
  /* [...snip...] */
};

extern int
__calc_delta(int delta_exec, unsigned long weight /* [...snip...] */);

/* !CONFIG_FAIR_GROUP_SCHED */
#define for_each_sched_entity(se) \
  for (; se; se = (struct sched_entity *)NULL)

extern struct cfs_rq*
cfs_rq_of(struct sched_entity* se);

extern int
__sched_period(unsigned long nr_running);

int
sched_slice(struct cfs_rq* cfs_rq, struct sched_entity* se)
{
  unsigned int nr_running = cfs_rq->nr_running;
  int slice;

  /* [...snip...] */

  slice = __sched_period(nr_running + !se->on_rq);

  for_each_sched_entity(se) {
    /* [...snip...] */
    cfs_rq = cfs_rq_of(se);
    /* [...snip...] */
    slice = __calc_delta(slice, se->load.weight);
  }

  /* [...snip...] */

  return slice;
}
