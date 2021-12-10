/* { dg-do compile } */
/* { dg-options "-O2 -fgnu89-inline" } */

typedef struct { } spinlock_t;
struct list_head {
 struct list_head *next, *prev;
};
struct __wait_queue_head {
 spinlock_t lock;
 struct list_head task_list;
};
typedef struct __wait_queue_head wait_queue_head_t;
static inline void init_waitqueue_head(wait_queue_head_t *q)
{
 q->lock = (spinlock_t) { };
 do { (&q->task_list)->next = (&q->task_list); (&q->task_list)->prev = (&q->task_list); } while (0);
}
struct timer_list {
 void (*function)(unsigned long);
};
struct rpc_task {
 struct timer_list tk_timer;
 wait_queue_head_t tk_wait;
};
static void
rpc_run_timer(struct rpc_task *task)
{
}
inline void
rpc_init_task(struct rpc_task *task)
{
 task->tk_timer.function = (void (*)(unsigned long)) rpc_run_timer;
 init_waitqueue_head(&task->tk_wait);
}

