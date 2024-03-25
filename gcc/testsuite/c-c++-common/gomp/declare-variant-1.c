int foo (int, int, int *);
int bar (int, int, int *);
#pragma omp declare variant (foo) \
  match (construct={parallel,for},\
	 device={isa(avx512f,avx512vl),kind(host,cpu)},\
	 implementation={vendor(score(0):gnu),unified_shared_memory},\
	 user={condition(score(0):0)})
#pragma omp declare variant (bar) \
  match (device={arch(x86_64,powerpc64),isa(avx512f,popcntb)}, \
	 implementation={atomic_default_mem_order(seq_cst),made_up_selector("foo", 13, "bar")}, \
	 user={condition(3-3)})
/* { dg-warning "unknown selector 'made_up_selector'" "" { target *-*-* } .-2 } */
int baz (int, int, int *);

int
qux (void)
{
  int i = 3;
  return baz (1, 2, &i);
}

int quux (int);

void
corge (void)
{
  int i;
  #pragma omp declare variant (quux) match (construct={parallel,for})
  extern int waldo (int);
  waldo (5);
  #pragma omp parallel for
  for (i = 0; i < 3; i++)
    waldo (6);
  #pragma omp parallel
  #pragma omp taskgroup
  #pragma omp for
  for (i = 0; i < 3; i++)
    waldo (7);
  #pragma omp parallel
  #pragma omp master    
  waldo (8);
}

#pragma omp declare variant (bar) match \
  (implementation={atomic_default_mem_order(relaxed), \
		   unified_address, unified_shared_memory, \
		   dynamic_allocators, reverse_offload})
int baz2 (int x, int y, int *z)
{
  return x + y + *z;
}

#pragma omp declare variant (bar) match \
  (implementation={atomic_default_mem_order(score(3): acq_rel)})
int baz3 (int, int, int *);
