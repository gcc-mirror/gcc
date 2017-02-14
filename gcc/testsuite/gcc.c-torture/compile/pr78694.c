/* PR target/78694.  */

enum
{
  MEMMODEL_RELAXED,
  MEMMODEL_ACQUIRE,
  PRIORITY_INSERT_END
};
enum
{
  PQ_CHILDREN,
  PQ_TASKGROUP
};
struct gomp_team_state
{
  struct gomp_team *team;
};
enum gomp_task_kind
{
  GOMP_TASK_UNDEFERRED,
  GOMP_TASK_WAITING
};
struct gomp_taskwait
{
  _Bool in_taskwait;
};
struct gomp_task
{
  struct gomp_task *parent;
  int children_queue;
  struct gomp_taskgroup *taskgroup;
  int dependers;
  struct gomp_taskwait taskwait;
  enum gomp_task_kind kind;
  _Bool in_tied_task;
} j, q, *n;
struct gomp_taskgroup
{
  _Bool in_taskgroup_wait;
  int num_children;
} l;
struct gomp_team
{
  int task_queue;
  int task_running_count;
};
struct gomp_thread
{
  struct gomp_team_state ts;
  struct gomp_task task;
} extern __thread a;

int b, c, d, e, f, g, h, i, k, m, o, p, r;

void priority_queue_next_task (struct gomp_task *, int, int);
int gomp_task_run_pre (struct gomp_task *, struct gomp_task, struct gomp_team);
void priority_queue_insert (int, struct gomp_task);
void priority_queue_insert2 (int, struct gomp_task, int, int, int);
void priority_queue_insert3 (int, struct gomp_task, int, int, int);
void gomp_sem_post (int);
void free (void *);

_Bool s;
int
GOMP_taskgroup_end ()
{
  struct gomp_thread *t = &a;
  struct gomp_team u = *t->ts.team;
  struct gomp_task *v = &t->task, *w;
  if (__atomic_load_n (&l.num_children, MEMMODEL_ACQUIRE))
    while (1)
      {
	if (l.num_children)
	  priority_queue_next_task (v, u.task_queue, r);
	else if (w)
	  free (w);
	if (n->kind == GOMP_TASK_WAITING)
	  {
	    s = gomp_task_run_pre (n, q, u);
	    if (__builtin_expect (s, 0))
	      {
		if (w)
		  free (w);
		goto finish_cancelled;
	      }
	    n = 0;
	    l.in_taskgroup_wait = 1;
	  }
	if (w)
	  {
	    t->task = *n;
	    if (__builtin_expect (p, 0))
	      if (o)
		t->task = *v;
	  }
	if (n)
	  {
	    struct gomp_task x = x;
	    for (; i; b++)
	      {
		struct gomp_task y = j;
		if (g)
		  continue;
		priority_queue_insert (PQ_CHILDREN, x);
		if (x.taskwait.in_taskwait)
		  priority_queue_insert2 (PQ_TASKGROUP, y, e, 0, d);
		if (h)
		  gomp_sem_post (f);
		priority_queue_insert3 (k, y, PRIORITY_INSERT_END, 0, d);
		++c;
	      }
	  }
      finish_cancelled:
	w = (struct gomp_task *) (n - u.task_running_count - v);
      }
  v->taskgroup = (struct gomp_taskgroup *) m;
  return 1;
}
