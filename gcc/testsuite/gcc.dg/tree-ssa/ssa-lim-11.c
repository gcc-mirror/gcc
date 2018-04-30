/* { dg-do compile } */
/* { dg-options "-O -fprofile-arcs -fprofile-update=single -fdump-tree-lim2-details" } */
/* { dg-require-profiling "-fprofile-generate" } */

struct thread_param
{
  long* buf;
  long iterations;
  long accesses;
} param;

void access_buf(struct thread_param* p)
{
  long i,j;
  long iterations = p->iterations;
  long accesses = p->accesses;
  for (i=0; i<iterations; i++)
    {
      long* pbuf = p->buf;
      for (j=0; j<accesses; j++)
	pbuf[j] += 1;
    }
}

/* { dg-final { scan-tree-dump-times "Executing store motion of __gcov0.access_buf\\\[\[12\]\\\] from loop 1" 2 "lim2" } } */
