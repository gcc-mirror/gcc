// { dg-do compile }

template<typename T>
void foo ()
{
#pragma omp for
  for (unsigned int i = 0; i < 10; i++); // { dg-warning "is unsigned" }
#pragma omp for
  for (int j = 0; ; j++); // { dg-error "missing controlling predicate" }
#pragma omp for
  for (int k = 0; k == 1; k++); // { dg-error "invalid controlling predicate" }
#pragma omp for
  for (int l = 0; l < 10; ); // { dg-error "missing increment expression" }
#pragma omp for
  for (int m = 0; m < 10; m *= 3); // Error here is emitted only during
				   // instantiation
#pragma omp for
  for (T n = 0; ; n++); // { dg-error "missing controlling predicate" }
#pragma omp for
  for (T o = 0; o == 1; o++); // Error here is emitted only during
			      // instantiation
#pragma omp for
  for (T p = 0; p < 10; ); // { dg-error "missing increment expression" }
#pragma omp for
  for (T q = 0; q < 10; q *= 3); // Error here is emitted only during
				 // instantiation
}

void bar ()
{
#pragma omp for
  for (int m = 0; m < 10; m *= 3); // { dg-error "invalid increment expression" }
}
