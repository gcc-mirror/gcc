void
foo (int *p)
{
  int i, j, k;
  #pragma omp parallel
  {
    #pragma omp for lastprivate (conditional: i)	/* { dg-warning "conditional 'lastprivate' on loop iterator 'i' ignored" } */
    for (i = 0; i < 32; i++)
      ;
    #pragma omp for collapse (3) lastprivate (conditional: i)	/* { dg-warning "conditional 'lastprivate' on loop iterator 'i' ignored" } */
    for (i = 0; i < 32; i++)
      for (j = 0; j < 32; ++j)
	for (k = 0; k < 2; ++k)
	  ;
    #pragma omp for collapse (3) lastprivate (conditional: j)	/* { dg-warning "conditional 'lastprivate' on loop iterator 'j' ignored" } */
    for (i = 0; i < 32; i++)
      for (j = 0; j < 32; ++j)
	for (k = 0; k < 2; ++k)
	  ;
    #pragma omp for collapse (3) lastprivate (conditional: k)	/* { dg-warning "conditional 'lastprivate' on loop iterator 'k' ignored" } */
    for (i = 0; i < 32; i++)
      for (j = 0; j < 32; ++j)
	for (k = 0; k < 2; ++k)
	  ;
  }
  #pragma omp parallel for lastprivate (conditional: i)	/* { dg-warning "conditional 'lastprivate' on loop iterator 'i' ignored" } */
  for (i = 0; i < 32; i++)
    ;
  #pragma omp parallel for collapse (3) lastprivate (conditional: i)	/* { dg-warning "conditional 'lastprivate' on loop iterator 'i' ignored" } */
  for (i = 0; i < 32; i++)
    for (j = 0; j < 32; ++j)
      for (k = 0; k < 2; ++k)
	;
  #pragma omp parallel for collapse (3) lastprivate (conditional: j)	/* { dg-warning "conditional 'lastprivate' on loop iterator 'j' ignored" } */
  for (i = 0; i < 32; i++)
    for (j = 0; j < 32; ++j)
      for (k = 0; k < 2; ++k)
	;
  #pragma omp parallel for collapse (3) lastprivate (conditional: k)	/* { dg-warning "conditional 'lastprivate' on loop iterator 'k' ignored" } */
  for (i = 0; i < 32; i++)
    for (j = 0; j < 32; ++j)
      for (k = 0; k < 2; ++k)
	;
}
