// { dg-do compile }

struct S;					// { dg-message "forward declaration" }
#pragma omp declare reduction (+:S:omp_out.s += omp_in.s) // { dg-error "incomplete type" }
struct S { int s; S () : s (1) {} };
#pragma omp declare reduction (*:S:omp_out.s *= omp_in.s)

void
foo ()
{
  S s;
  #pragma omp parallel reduction (S::~S:s)	// { dg-error "invalid reduction-identifier" }
  s.s = 1;
}
