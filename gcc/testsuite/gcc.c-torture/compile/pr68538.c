struct percpu_counter {
	signed long long count;
};
struct blkg_rwstat {
	struct percpu_counter cpu_cnt[4];
};
struct cfq_group {
	struct blkg_rwstat service_time;
};
struct cfq_queue {
	struct cfq_group *group;
};
struct request {
	struct cfq_queue *active_queue;
	unsigned long long cmd_flags;
	void *priv;
};
static void blkg_rwstat_add(struct blkg_rwstat *rwstat, int rw, unsigned long long val)
{
	struct percpu_counter *cnt;
	if (rw & 1)
		cnt = &rwstat->cpu_cnt[1];
	else
		cnt = &rwstat->cpu_cnt[0];
	cnt->count += val;
	if (rw & 2)
		cnt = &rwstat->cpu_cnt[2];
	else
		cnt = &rwstat->cpu_cnt[3];
	cnt->count += val;
}
extern unsigned long long rq_start_time_ns(void);
extern unsigned long long rq_io_start_time_ns(void);
extern int rq_is_sync(void);
extern void cfq_arm_slice_timer(void);
void cfq_completed_request(struct request *rq)
{
	struct cfq_queue *queue = rq->priv;
	int sync = rq_is_sync();
	struct cfq_group *group = queue->group;
	long long start_time = rq_start_time_ns();
	long long io_start_time = rq_io_start_time_ns();
	int rw = rq->cmd_flags;

	if (io_start_time < 1)
		blkg_rwstat_add(&group->service_time, rw, 1 - io_start_time);
	blkg_rwstat_add(0, rw, io_start_time - start_time);

	if (rq->active_queue == queue && sync)
		cfq_arm_slice_timer();
}

