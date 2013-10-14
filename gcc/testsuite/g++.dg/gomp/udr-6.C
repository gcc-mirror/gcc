// { dg-do compile }

struct A { int a; A () : a (0) {} };
struct B { int b; B () : b (0) {} };
struct C : public A, B { int c; C () : c (0) {} };
struct D { int d; D () : d (0) {} };
struct E { int e; E () : e (0) {} };
struct F : public D, E { int f; F () : f (0) {} };
struct G : public C, F { int g; G () : g (0) {} };

#pragma omp declare reduction (+: A : omp_out.a += omp_in.a) // { dg-message "operator" }
#pragma omp declare reduction (+: B : omp_out.b += omp_in.b) // { dg-message "operator" }
#pragma omp declare reduction (+: D : omp_out.d += omp_in.d)
#pragma omp declare reduction (+: E : omp_out.e += omp_in.e)
#pragma omp declare reduction (+: F : omp_out.f += omp_in.f) // { dg-message "operator" }

void
f1 ()
{
  G g;
  #pragma omp parallel reduction (+: g)	// { dg-error "user defined reduction lookup is ambiguous" }
  {
    g.g++;
  }
}

#pragma omp declare reduction (*: A : omp_out.a += omp_in.a)
#pragma omp declare reduction (*: B : omp_out.b += omp_in.b)
#pragma omp declare reduction (*: D : omp_out.d += omp_in.d)
#pragma omp declare reduction (*: E : omp_out.e += omp_in.e)
#pragma omp declare reduction (*: F : omp_out.f += omp_in.f)
#pragma omp declare reduction (*: G : omp_out.g += omp_in.g)

void
f2 ()
{
  G g;
  #pragma omp parallel reduction (*: g)
  {
    g.g++;
  }
}

#pragma omp declare reduction (|: A : omp_out.a += omp_in.a)
#pragma omp declare reduction (|: B : omp_out.b += omp_in.b)
#pragma omp declare reduction (|: C : omp_out.c += omp_in.c) // { dg-message "operator" }
#pragma omp declare reduction (|: D : omp_out.d += omp_in.d)
#pragma omp declare reduction (|: E : omp_out.e += omp_in.e)
#pragma omp declare reduction (|: F : omp_out.f += omp_in.f) // { dg-message "operator" }

void
f3 ()
{
  G g;
  #pragma omp parallel reduction (|: g)	// { dg-error "user defined reduction lookup is ambiguous" }
  {
    g.g++;
  }
}
