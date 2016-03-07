/* PR middle-end/61486 */
/* { dg-do compile } */
/* { dg-require-effective-target alloca } */

#pragma omp declare target
void dosomething (int *a, int n, int m);
#pragma omp end declare target

void
test (int n, int o, int p, int q, int r, int s, int *pp)
{
  int a[o], i, j;
  #pragma omp target data device (n + 1) if (n != 6) map (tofrom: n, r)
  {
    #pragma omp target device (n + 1) if (n != 6) map (from: n) map (alloc: a[2:o-2])
      dosomething (a, n, 0);
    #pragma omp target teams device (n + 1) num_teams (n + 4) thread_limit (n * 2) \
    	if (n != 6)map (from: n) map (alloc: a[2:o-2]) default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r)
    {
      r = r + 1;
      p = q;
      dosomething (a, n, p + q);
    }
    #pragma omp target teams distribute device (n + 1) num_teams (n + 4) collapse (2) \
    	if (n != 6)map (from: n) map (alloc: a[2:o-2]) default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	thread_limit (n * 2) dist_schedule (static, 4)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    dosomething (a, n, p + q);
	  }
    #pragma omp target teams distribute device (n + 1) num_teams (n + 4) \
    	if (n != 6)map (from: n) map (alloc: a[2:o-2]) default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	thread_limit (n * 2) dist_schedule (static, 4)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    dosomething (a, n, p + q);
	  }
    #pragma omp target teams distribute parallel for device (n + 1) num_teams (n + 4) \
    	if (n != 6)map (from: n) map (alloc: a[2:o-2]) default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	thread_limit (n * 2) dist_schedule (static, 4) collapse (2) \
    	num_threads (n + 4) proc_bind (spread) lastprivate (s) \
    	schedule (static, 8)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    dosomething (a, n, p + q);
	    p = q;
	    s = i * 10 + j;
	  }
    #pragma omp target teams distribute parallel for device (n + 1) num_teams (n + 4) \
    	if (n != 6)map (from: n) map (alloc: a[2:o-2]) default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	thread_limit (n * 2) dist_schedule (static, 4) num_threads (n + 4) \
    	proc_bind (master) lastprivate (s) schedule (static, 8)
      for (i = 0; i < 10; i++)
	{
	  for (j = 0; j < 10; j++)
	    {
	      r = r + 1;
	      p = q;
	      dosomething (a, n, p + q);
	    }
	  p = q;
	  s = i * 10;
	}
    #pragma omp target teams distribute parallel for simd device (n + 1) \
    	if (n != 6)map (from: n) map (alloc: a[2:o-2]) default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	thread_limit (n * 2) dist_schedule (static, 4) collapse (2) \
    	num_threads (n + 4) proc_bind (spread) lastprivate (s) \
    	schedule (static, 8) num_teams (n + 4) safelen(8)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    a[2+i*10+j] = p + q;
	    s = i * 10 + j;
	  }
    #pragma omp target teams distribute parallel for simd device (n + 1) \
    	if (n != 6)map (from: n) map (alloc: a[2:o-2]) default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	thread_limit (n * 2) dist_schedule (static, 4) num_threads (n + 4) \
    	proc_bind (master) lastprivate (s) schedule (static, 8) \
    	num_teams (n + 4) safelen(16) linear(i:1) aligned (pp:4)
      for (i = 0; i < 10; i++)
	{
	  r = r + 1;
	  p = q;
	  a[2+i] = p + q;
	  s = i * 10;
	}
    #pragma omp target teams distribute simd device (n + 1) \
    	if (n != 6)map (from: n) map (alloc: a[2:o-2]) default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	thread_limit (n * 2) dist_schedule (static, 4) collapse (2) \
    	lastprivate (s) num_teams (n + 4) safelen(8)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    a[2+i*10+j] = p + q;
	    s = i * 10 + j;
	  }
    #pragma omp target teams distribute simd device (n + 1) \
    	if (n != 6)map (from: n) map (alloc: a[2:o-2]) default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	thread_limit (n * 2) dist_schedule (static, 4) lastprivate (s) \
    	num_teams (n + 4) safelen(16) linear(i:1) aligned (pp:4)
      for (i = 0; i < 10; i++)
	{
	  r = r + 1;
	  p = q;
	  a[2+i] = p + q;
	  s = i * 10;
	}
    #pragma omp target device (n + 1) if (n != 6)map(from:n) map(alloc:a[2:o-2])
    #pragma omp teams num_teams (n + 4) thread_limit (n * 2) default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r)
    {
      r = r + 1;
      p = q;
      dosomething (a, n, p + q);
    }
    #pragma omp target device (n + 1) if (n != 6)map(from:n) map(alloc:a[2:o-2])
    #pragma omp teams distribute num_teams (n + 4) collapse (2) default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	thread_limit (n * 2) dist_schedule (static, 4)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    dosomething (a, n, p + q);
	  }
    #pragma omp target device (n + 1) if (n != 6)map(from:n) map(alloc:a[2:o-2])
    #pragma omp teams distribute num_teams (n + 4) default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	thread_limit (n * 2) dist_schedule (static, 4)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    dosomething (a, n, p + q);
	  }
    #pragma omp target device (n + 1) if (n != 6)map(from:n) map(alloc:a[2:o-2])
    #pragma omp teams distribute parallel for num_teams (n + 4) if (n != 6) \
	default(shared) private (p) firstprivate (q) shared (n) reduction (+: r) \
    	thread_limit (n * 2) dist_schedule (static, 4) collapse (2) \
    	num_threads (n + 4) proc_bind (spread) lastprivate (s) \
    	schedule (static, 8)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    dosomething (a, n, p + q);
	    p = q;
	    s = i * 10 + j;
	  }
    #pragma omp target device (n + 1) if (n != 6)map(from:n) map(alloc:a[2:o-2])
    #pragma omp teams distribute parallel for num_teams (n + 4) if (n != 6) \
	default(shared) private (p) firstprivate (q) shared (n) reduction (+: r) \
    	thread_limit (n * 2) dist_schedule (static, 4) num_threads (n + 4) \
    	proc_bind (master) lastprivate (s) schedule (static, 8)
      for (i = 0; i < 10; i++)
	{
	  for (j = 0; j < 10; j++)
	    {
	      r = r + 1;
	      p = q;
	      dosomething (a, n, p + q);
	    }
	  p = q;
	  s = i * 10;
	}
    #pragma omp target device (n + 1) if (n != 6)map(from:n) map(alloc:a[2:o-2])
    #pragma omp teams distribute parallel for simd if (n != 6)default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	thread_limit (n * 2) dist_schedule (static, 4) collapse (2) \
    	num_threads (n + 4) proc_bind (spread) lastprivate (s) \
    	schedule (static, 8) num_teams (n + 4) safelen(8)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    a[2+i*10+j] = p + q;
	    s = i * 10 + j;
	  }
    #pragma omp target device (n + 1) if (n != 6)map(from:n) map(alloc:a[2:o-2])
    #pragma omp teams distribute parallel for simd if (n != 6)default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	thread_limit (n * 2) dist_schedule (static, 4) num_threads (n + 4) \
    	proc_bind (master) lastprivate (s) schedule (static, 8) \
    	num_teams (n + 4) safelen(16) linear(i:1) aligned (pp:4)
      for (i = 0; i < 10; i++)
	{
	  r = r + 1;
	  p = q;
	  a[2+i] = p + q;
	  s = i * 10;
	}
    #pragma omp target device (n + 1) if (n != 6)map(from:n) map(alloc:a[2:o-2])
    #pragma omp teams distribute simd default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	thread_limit (n * 2) dist_schedule (static, 4) collapse (2) \
    	lastprivate (s) num_teams (n + 4) safelen(8)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    a[2+i*10+j] = p + q;
	    s = i * 10 + j;
	  }
    #pragma omp target device (n + 1) if (n != 6)map(from:n) map(alloc:a[2:o-2])
    #pragma omp teams distribute simd default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	thread_limit (n * 2) dist_schedule (static, 4) lastprivate (s) \
    	num_teams (n + 4) safelen(16) linear(i:1) aligned (pp:4)
      for (i = 0; i < 10; i++)
	{
	  r = r + 1;
	  p = q;
	  a[2+i] = p + q;
	  s = i * 10;
	}
    #pragma omp target teams device (n + 1) if (n != 6)map(from:n) map(alloc:a[2:o-2]) \
	num_teams (n + 4) thread_limit (n * 2)default(shared) shared(n) \
	private (p) reduction (+: r)
    #pragma omp distribute collapse (2) dist_schedule (static, 4) firstprivate (q)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    dosomething (a, n, p + q);
	  }
    #pragma omp target teams device (n + 1) if (n != 6)map(from:n) map(alloc:a[2:o-2]) \
	num_teams (n + 4) thread_limit (n * 2) shared(n) private(p) reduction (+ : r) \
	default(shared)
    #pragma omp distribute dist_schedule (static, 4) firstprivate (q)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    dosomething (a, n, p + q);
	  }
    #pragma omp target teams device (n + 1) if (n != 6)map(from:n) map(alloc:a[2:o-2]) \
	num_teams (n + 4) thread_limit (n * 2)
    #pragma omp distribute parallel for if (n != 6) \
	default(shared) private (p) firstprivate (q) shared (n) reduction (+: r) \
    	collapse (2) dist_schedule (static, 4) \
    	num_threads (n + 4) proc_bind (spread) lastprivate (s) \
    	schedule (static, 8)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    dosomething (a, n, p + q);
	    p = q;
	    s = i * 10 + j;
	  }
    #pragma omp target teams device (n + 1) if (n != 6)map(from:n) map(alloc:a[2:o-2]) \
	num_teams (n + 4) thread_limit (n * 2)
    #pragma omp distribute parallel for if (n != 6) \
	default(shared) private (p) firstprivate (q) shared (n) reduction (+: r) \
    	num_threads (n + 4) dist_schedule (static, 4) \
    	proc_bind (master) lastprivate (s) schedule (static, 8)
      for (i = 0; i < 10; i++)
	{
	  for (j = 0; j < 10; j++)
	    {
	      r = r + 1;
	      p = q;
	      dosomething (a, n, p + q);
	    }
	  p = q;
	  s = i * 10;
	}
    #pragma omp target teams device (n + 1) if (n != 6)map(from:n) map(alloc:a[2:o-2]) \
	num_teams (n + 4) thread_limit (n * 2)
    #pragma omp distribute parallel for simd if (n != 6)default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	collapse (2) dist_schedule (static, 4) \
    	num_threads (n + 4) proc_bind (spread) lastprivate (s) \
    	schedule (static, 8) safelen(8)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    a[2+i*10+j] = p + q;
	    s = i * 10 + j;
	  }
    #pragma omp target teams device (n + 1) if (n != 6)map(from:n) map(alloc:a[2:o-2]) \
	num_teams (n + 4) thread_limit (n * 2)
    #pragma omp distribute parallel for simd if (n != 6)default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	num_threads (n + 4) dist_schedule (static, 4) \
    	proc_bind (master) lastprivate (s) schedule (static, 8) \
    	safelen(16) linear(i:1) aligned (pp:4)
      for (i = 0; i < 10; i++)
	{
	  r = r + 1;
	  p = q;
	  a[2+i] = p + q;
	  s = i * 10;
	}
    #pragma omp target teams device (n + 1) if (n != 6)map(from:n) map(alloc:a[2:o-2]) \
	num_teams (n + 4) thread_limit (n * 2) default(shared) shared(n) private(p) \
	reduction(+:r)
    #pragma omp distribute simd private (p) firstprivate (q) reduction (+: r) \
    	collapse (2) dist_schedule (static, 4) lastprivate (s) safelen(8)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    a[2+i*10+j] = p + q;
	    s = i * 10 + j;
	  }
    #pragma omp target teams device (n + 1) if (n != 6)map(from:n) map(alloc:a[2:o-2]) \
	num_teams (n + 4) thread_limit (n * 2) default(shared) shared(n) private(p) \
	reduction(+:r)
    #pragma omp distribute simd private (p) firstprivate (q) reduction (+: r) \
    	lastprivate (s) dist_schedule (static, 4) safelen(16) linear(i:1) aligned (pp:4)
      for (i = 0; i < 10; i++)
	{
	  r = r + 1;
	  p = q;
	  a[2+i] = p + q;
	  s = i * 10;
	}
  }
}

int q, i, j;

#pragma omp declare target
int s;

void
test2 (int n, int o, int p, int r, int *pp)
{
  int a[o];
    #pragma omp distribute collapse (2) dist_schedule (static, 4) firstprivate (q)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    dosomething (a, n, p + q);
	  }
    #pragma omp distribute dist_schedule (static, 4) firstprivate (q)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    dosomething (a, n, p + q);
	  }
    #pragma omp distribute parallel for if (n != 6) \
	default(shared) private (p) firstprivate (q) shared (n) reduction (+: r) \
    	collapse (2) dist_schedule (static, 4) \
    	num_threads (n + 4) proc_bind (spread) lastprivate (s) \
    	schedule (static, 8)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    dosomething (a, n, p + q);
	    p = q;
	    s = i * 10 + j;
	  }
    #pragma omp distribute parallel for if (n != 6) \
	default(shared) private (p) firstprivate (q) shared (n) reduction (+: r) \
    	num_threads (n + 4) dist_schedule (static, 4) \
    	proc_bind (master) lastprivate (s) schedule (static, 8)
      for (i = 0; i < 10; i++)
	{
	  for (j = 0; j < 10; j++)
	    {
	      r = r + 1;
	      p = q;
	      dosomething (a, n, p + q);
	    }
	  p = q;
	  s = i * 10;
	}
    #pragma omp distribute parallel for simd if (n != 6)default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	collapse (2) dist_schedule (static, 4) \
    	num_threads (n + 4) proc_bind (spread) lastprivate (s) \
    	schedule (static, 8) safelen(8)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    a[2+i*10+j] = p + q;
	    s = i * 10 + j;
	  }
    #pragma omp distribute parallel for simd if (n != 6)default(shared) \
    	private (p) firstprivate (q) shared (n) reduction (+: r) \
    	num_threads (n + 4) dist_schedule (static, 4) \
    	proc_bind (master) lastprivate (s) schedule (static, 8) \
    	safelen(16) linear(i:1) aligned (pp:4)
      for (i = 0; i < 10; i++)
	{
	  r = r + 1;
	  p = q;
	  a[2+i] = p + q;
	  s = i * 10;
	}
    #pragma omp distribute simd private (p) firstprivate (q) reduction (+: r) \
    	collapse (2) dist_schedule (static, 4) lastprivate (s) safelen(8)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  {
	    r = r + 1;
	    p = q;
	    a[2+i*10+j] = p + q;
	    s = i * 10 + j;
	  }
    #pragma omp distribute simd private (p) firstprivate (q) reduction (+: r) \
    	lastprivate (s) dist_schedule (static, 4) safelen(16) linear(i:1) aligned (pp:4)
      for (i = 0; i < 10; i++)
	{
	  r = r + 1;
	  p = q;
	  a[2+i] = p + q;
	  s = i * 10;
	}
}
#pragma omp end declare target
