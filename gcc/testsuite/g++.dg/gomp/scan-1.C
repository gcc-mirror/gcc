// { dg-do compile { target c++11 } }

struct S { S (); ~S (); S &operator = (const S &) = delete; int s; };	// { dg-message "declared here" }
#pragma omp declare reduction (+ : S : omp_out.s += omp_in.s)

S s;

void
foo (void)
{
  #pragma omp simd reduction (+: s)
  for (int i = 0; i < 64; ++i)
    s.s += i;
}

void
bar (int *x)
{
  #pragma omp simd reduction (inscan, +: s)	// { dg-error "use of deleted function" }
  for (int i = 0; i < 64; ++i)
    {
      s.s += i;
      #pragma omp scan inclusive (s)		// { dg-error "" }
      x[i] = s.s;
    }
}
