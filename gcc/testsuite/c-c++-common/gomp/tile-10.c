void
test1 (void)
{
  int i, j, k, l;
  #pragma omp for collapse(4) private (i, j, k, l) /* { dg-error "the same loop iteration variables 'i' used in multiple associated loops" "" { target c } } */
  for (i = 0; i < 1024; ++i) /* { dg-error "the same loop iteration variables 'i' used in multiple associated loops" "" { target c++ } } */
    #pragma omp tile sizes (2, 2, 2)
    for (j = 0; j < 1024; ++j)
      #pragma omp tile sizes (3, 3)
      for (k = 0; k < 1024; ++k)
	#pragma omp tile sizes (4)
	for (i = 0; i < 1024; ++i)
	  ;
}

void
test2 (void)
{
  long long i;
  int j, k, l;
  #pragma omp for collapse(4) private (i, j, k, l) /* { dg-error "outer iteration variable 'i' used in initializer expression has type other than 'int'" "" { target c } } */
  for (i = 0; i < 1024; ++i) /* { dg-error "outer iteration variable 'i' used in initializer expression has type other than 'int'" "" { target c++ } } */
    #pragma omp tile sizes (2, 2, 2)
    for (j = 0; j < 1024; ++j)
      #pragma omp tile sizes (3, 3)
      for (k = 0; k < 1024; ++k)
	#pragma omp tile sizes (4)
	for (l = i; l < 1024; ++l)
	  ;
}

void
test3 (void)
{
  int i, j, k, l;
  #pragma omp for collapse(4) private (i, j, k, l)
  for (i = 0; i < 1024; ++i)
    #pragma omp tile sizes (2, 2, 2)
    for (j = 0; j < 1024; ++j)
      #pragma omp tile sizes (3, 3)
      for (k = 0; k < 1024; ++k)
	#pragma omp tile sizes (4)
	for (l = 0; l < 7 * i * i; ++l) /* { dg-error "condition expression refers to iteration variable 'i'" } */
	  ;
}

void
test4 (void)
{
  int i, j, k, l;
  #pragma omp for collapse(4) private (i, j, k, l)
  for (i = 0; i < 1024; ++i)
    #pragma omp tile sizes (2, 2, 2)
    for (j = 0; j < 1024; ++j)
      #pragma omp tile sizes (3, 3)
      for (k = 0; k < 1024; ++k)
	#pragma omp tile sizes (4)
	for (l = i * i; l < 1024; ++l) /* { dg-error "initializer expression refers to iteration variable 'i'" } */
	  ;
}

void
test5 (void)
{
  int i, j, k, l;
  #pragma omp for collapse(4) private (i, j, k, l)
  for (i = 0; i < 1024; ++i)
    #pragma omp tile sizes (2, 2, 2) /* { dg-error "increment expression refers to iteration variable 'j'" "" { target c } } */
    for (j = 0; j < 1024; ++j) /* { dg-error "increment expression refers to iteration variable 'j'" "" { target c++ } } */
      #pragma omp tile sizes (3, 3)
      for (k = 0; k < 1024; ++k)
	#pragma omp tile sizes (4)
	for (l = 0; l < 1024; l += j)
	  ;
}

void
test6 (void)
{
  int i, j, k, l;
  #pragma omp for collapse(4) private (i, j, k, l)
  for (i = 0; i < 1024; ++i)
    #pragma omp tile sizes (2, 2, 2)
    for (j = 0; j < 1024; ++j)
      #pragma omp tile sizes (3, 3)
      for (k = 0; k < 1024; ++k)
	#pragma omp tile sizes (4)
	for (l = 0; l < i - 2; ++l) /* { dg-message "non-rectangular loops from generated loops unsupported" } */
	  ;
}
