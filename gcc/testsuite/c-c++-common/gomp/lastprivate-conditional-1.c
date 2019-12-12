void
foo (int *p)
{
  int a = -1, b = -1, c = -1, d = -1, e = -1, f = -1, g = -1, h = -1;
  int i;
  #pragma omp teams
  {
    #pragma omp distribute lastprivate (conditional: a) /* { dg-error "conditional 'lastprivate' clause on 'distribute' construct" } */
    for (i = 0; i < 32; i++)
      if (p[i])
	a = i;
    #pragma omp distribute simd lastprivate (conditional: b) /* { dg-error "conditional 'lastprivate' clause on 'distribute' construct" } */
    for (i = 0; i < 32; i++)
      if (p[i])
	b = i;
    #pragma omp distribute parallel for lastprivate (conditional: c) /* { dg-error "conditional 'lastprivate' clause on 'distribute' construct" } */
    for (i = 0; i < 32; i++)
      if (p[i])
	c = i;
    #pragma omp distribute parallel for simd lastprivate (conditional: d) /* { dg-error "conditional 'lastprivate' clause on 'distribute' construct" } */
    for (i = 0; i < 32; i++)
      if (p[i])
	d = i;
  }
  #pragma omp teams distribute parallel for lastprivate (conditional: e) /* { dg-error "conditional 'lastprivate' clause on 'distribute' construct" } */
  for (i = 0; i < 32; i++)
    if (p[i])
      e = i;
  #pragma omp parallel
  {
    #pragma omp master
    #pragma omp taskloop lastprivate (conditional: f) /* { dg-error "conditional 'lastprivate' clause on 'taskloop' construct" } */
    for (i = 0; i < 32; i++)
      if (p[i])
	f = i;
    #pragma omp master taskloop simd lastprivate (conditional: g) /* { dg-error "conditional 'lastprivate' clause on 'taskloop' construct" } */
    for (i = 0; i < 32; i++)
      if (p[i])
	g = i;
  }
  #pragma omp parallel master taskloop simd lastprivate (conditional: h) /* { dg-error "conditional 'lastprivate' clause on 'taskloop' construct" } */
  for (i = 0; i < 32; i++)
    if (p[i])
      h = i;
}

struct S { int a, b; };

void
bar (int *p)
{
  struct S s = { -1, -1 }, t = { 1, 2 };
  int i;
  #pragma omp parallel for lastprivate (conditional: s) /* { dg-error "non-scalar variable 's' in conditional 'lastprivate' clause" } */
  for (i = 0; i < 32; i++)
    if (p[i])
      {
	struct S u = t;
	u.b = i;
	s = u;
      }
}

/* { dg-prune-output "not supported yet" } */
