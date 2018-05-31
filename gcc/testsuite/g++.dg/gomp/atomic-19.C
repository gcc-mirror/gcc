int i;

template <int N, typename T>
void
foo (T x)
{
  #pragma omp atomic hint (x)		// { dg-error "must be integral" }
  i = i + 1;
  #pragma omp atomic hint (N + i)	// { dg-error "constant integer expression" }
  i = i + 1;
}

void
bar ()
{
  foo <0, float> (1.0f);
}
