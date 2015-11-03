int test1()
{
  int i, j, k, b[10];
  int a[30];
  double d;
  float r;
  i = 0;
  #pragma acc loop
  while(1)  /* { dg-error "for statement expected" } */
    {
      if (i > 0) break; 
      i = i + 1;
    }
  i = 0;
  #pragma acc loop
  for(;;)  /* { dg-error "expected iteration declaration or initialization" } */
    {
      if (i > 0) break; /* { dg-error "break statement used" } */
      i = i + 1;
    }
  i = 0;
  #pragma acc loop
  do  /* { dg-error "for statement expected" } */
    {
      i = i + 1;
    }
  while (i < 4);
  #pragma acc loop
  while (i < 8)  /* { dg-error "for statement expected" } */
    {
      i = i + 1;
    }
  #pragma acc loop
  for (d = 1; d < 30; d+= 6)  /* { dg-error "invalid type for iteration variable" } */
    {
      i = d;
      a[i] = 1;
    }
  #pragma acc loop /* { dg-error "loop directive must be associated with an OpenACC compute region" } */
  for (i = 1; i < 30; i++ )
    if (i == 16) break; /* { dg-error "break statement used" } */

/* different types of for loop are allowed */
  #pragma acc loop /* { dg-error "loop directive must be associated with an OpenACC compute region" } */
  for (i = 1; i < 10; i++)
    {
    }
  #pragma acc loop /* { dg-error "loop directive must be associated with an OpenACC compute region" } */
  for (i = 1; i < 10; i+=2)
    {
      a[i] = i;
    }

  /* after loop directive must be loop */
  #pragma acc loop
    a[1] = 1; /* { dg-error "for statement expected" } */
    for (i = 1; i < 10; i++)
      ;
  /* combined directives may be used*/
  #pragma acc parallel loop
  for(i = 1; i < 10; i++)
    {
    }
  #pragma acc kernels loop
  for(i = 1; i < 10; i++)
    {
    }
  return 0;
}

// PR64765
void PR64765(float *f, double *r) {
  int i;
  #pragma acc kernels loop create(f) copy(r)
  for(i = 64; i < 76; i += 5) {}
}
