/* Testcase by Martin Michlmayr <tbm@cyrius.com> */
/* { dg-do compile } */

struct list_head
{
  struct list_head *prev;
};
struct prio_array
{
  struct list_head queue[100];
};
struct rq
{
  struct prio_array *active, arrays[2];
} per_cpu__runqueues;

void sched_init (unsigned long __ptr)
{
  int j, k;
  struct prio_array *array;
  struct rq *rq;
  rq = (&(*( { (typeof (&per_cpu__runqueues)) (__ptr); } )));
  for (j = 0; j < 2; j++)
  {
    array = rq->arrays + j;
    for (k = 0; k < 100; k++)
        (array->queue + k)->prev = array->queue;
  }
}

/* { dg-final { cleanup-tree-dump "vect" } } */
