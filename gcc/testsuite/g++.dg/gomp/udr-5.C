// { dg-do compile }

struct S
{
  int s;
  S () : s (0) {}
private:
  #pragma omp declare reduction (+:S:omp_out.s += omp_in.s)	// { dg-error "is private" }
protected:
  #pragma omp declare reduction (-:S:omp_out.s += omp_in.s)	// { dg-error "is protected" }
};

struct T : public S
{
  void foo ()
  {
    S s;
    #pragma omp parallel reduction (S::operator +:s)	// { dg-error "within this context" }
    s.s = 1;
    S t;
    #pragma omp parallel reduction (S::operator -:t)
    t.s = 1;
    S u;
    #pragma omp parallel reduction (+:u)		// { dg-error "within this context" }
    u.s = 1;
    S v;
    #pragma omp parallel reduction (-:v)
    v.s = 1;
  }
};

void
foo ()
{
  S s;
  #pragma omp parallel reduction (S::operator +:s)	// { dg-error "within this context" }
  s.s = 1;
  S t;
  #pragma omp parallel reduction (S::operator -:t)	// { dg-error "within this context" }
  t.s = 1;
}
