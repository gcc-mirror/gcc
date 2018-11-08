// { dg-do compile }
// { dg-options "-fopenmp" }

int d;

struct A
{
  A () : a(2), b(3), c(d) {}
  A (int x) : a(2), b(x), c(d) {}
  int a;
  A (const A &);
  A &operator= (const A &);
  const A &operator= (const A &) const;
  mutable int b;
  int &c;
};

struct B : public A
{
  B () : h(5) {}
  ~B ();
  B (const B &);
  A e;
  mutable A f;
  const A g;
  const int h;
  int m1 ();
  int m2 ();
  int m3 () const;
  int m4 () const;
};

void foo (A &);

#pragma omp declare reduction (+:A:omp_out.b += omp_in.b) initializer (foo (omp_priv))

int
B::m1 ()
{
  #pragma omp parallel private (a, b, c, e, f, g)
    ;
  #pragma omp parallel firstprivate (a, b, c, e, f, g)
    ;
  #pragma omp parallel for lastprivate (a, b, c, e, f, g)
    for (int i = 0; i < 10; i++)
      ;
  #pragma omp simd linear (a, b, c : 1)
    for (int i = 0; i < 10; i++)
      {
	a++;
	b++;
	c++;
      }
  #pragma omp parallel for reduction (+:a, b, c, e, f)
    for (int i = 0; i < 10; i++)
      ;
  return 0;
}

int
B::m2 ()
{
  #pragma omp parallel private (h)	// { dg-error "may appear only in .shared. or .firstprivate. clauses" }
    ;
  #pragma omp parallel firstprivate (h)
    ;
  #pragma omp parallel for lastprivate (h)	// { dg-error "may appear only in .shared. or .firstprivate. clauses" }
    for (int i = 0; i < 10; i++)
      ;
  #pragma omp simd linear (h : 1)	// { dg-error "may appear only in .shared. or .firstprivate. clauses" }
    for (int i = 0; i < 10; i++)
      ;
  #pragma omp parallel for reduction (+:h)	// { dg-error "may appear only in .shared. or .firstprivate. clauses" }
    for (int i = 0; i < 10; i++)
      ;
  #pragma omp parallel for reduction (+:g)	// { dg-error "has const type for .reduction." }
    for (int i = 0; i < 10; i++)
      ;
  #pragma omp parallel shared (a)
    ;
  #pragma omp parallel shared (b)
    ;
  #pragma omp parallel shared (c)
    ;
  #pragma omp parallel shared (e)
    ;
  #pragma omp parallel shared (f)
    ;
  #pragma omp parallel shared (g)
    ;
  #pragma omp parallel shared (h)
    ;
  return 0;
}

int
B::m3 () const
{
  #pragma omp parallel private (b, c, e, f, g)
    ;
  #pragma omp parallel firstprivate (b, c, e, f, g)
    ;
  #pragma omp parallel for lastprivate (b, c, e, f, g)
    for (int i = 0; i < 10; i++)
      ;
  #pragma omp simd linear (b, c : 1)
    for (int i = 0; i < 10; i++)
      {
	b++;
	c++;
      }
  #pragma omp parallel for reduction (+:b, c, f)
    for (int i = 0; i < 10; i++)
      ;
  return 0;
}

int
B::m4 () const
{
  #pragma omp parallel private (a)	// { dg-error "may appear only in .shared. or .firstprivate. clauses" }
    ;
  #pragma omp parallel firstprivate (a)
    ;
  #pragma omp parallel for lastprivate (a)	// { dg-error "may appear only in .shared. or .firstprivate. clauses" }
    for (int i = 0; i < 10; i++)
      ;
  #pragma omp simd linear (a : 1)	// { dg-error "may appear only in .shared. or .firstprivate. clauses" }
    for (int i = 0; i < 10; i++)
      ;
  #pragma omp parallel for reduction (+:a)	// { dg-error "may appear only in .shared. or .firstprivate. clauses" }
    for (int i = 0; i < 10; i++)
      ;
  #pragma omp parallel private (h)	// { dg-error "may appear only in .shared. or .firstprivate. clauses" }
    ;
  #pragma omp parallel firstprivate (h)
    ;
  #pragma omp parallel for lastprivate (h)	// { dg-error "may appear only in .shared. or .firstprivate. clauses" }
    for (int i = 0; i < 10; i++)
      ;
  #pragma omp simd linear (h : 1)	// { dg-error "may appear only in .shared. or .firstprivate. clauses" }
    for (int i = 0; i < 10; i++)
      ;
  #pragma omp parallel for reduction (+:h)	// { dg-error "may appear only in .shared. or .firstprivate. clauses" }
    for (int i = 0; i < 10; i++)
      ;
  #pragma omp parallel for reduction (+:e)	// { dg-error "has const type for .reduction." }
    for (int i = 0; i < 10; i++)
      ;
  #pragma omp parallel for reduction (+:g)	// { dg-error "has const type for .reduction." }
    for (int i = 0; i < 10; i++)
      ;
  #pragma omp parallel shared (a)
    ;
  #pragma omp parallel shared (b)
    ;
  #pragma omp parallel shared (c)
    ;
  #pragma omp parallel shared (e)
    ;
  #pragma omp parallel shared (f)
    ;
  #pragma omp parallel shared (g)
    ;
  #pragma omp parallel shared (h)
    ;
  return 0;
}
