// { dg-do compile }

void f1(void)
{
  #pragma omp barrier a		// { dg-error "expected end of line" }
}

void f3(bool p)
{
  if (p)
    #pragma omp barrier		// { dg-error "compound statements" }
}				// { dg-error "" }
