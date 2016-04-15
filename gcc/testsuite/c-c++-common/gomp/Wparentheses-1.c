/* PR c/70436 */
/* { dg-additional-options "-Wparentheses" } */

int a, b, c;
void bar (void);
void baz (void);

void
f1 (void)
{
  int i, j;

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp for
    for (i = 0; i < 10; i++)
      if (b)
	bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    while (1)
      #pragma omp for
      for (i = 0; i < 10; i++)
	if (b)
	  bar ();
	else
	  baz ();

  if (a) /* { dg-warning "ambiguous" } */
    for (i = 0; i < 10; i++)
      #pragma omp for
      for (j = 0; j < 10; j++)
	if (b)
	  bar ();
  else
    baz ();

  if (a)
    #pragma omp for
    for (i = 0; i < 10; i++)
      if (b) /* { dg-warning "ambiguous" } */
	#pragma omp parallel for
	for (j = 0; j < 10; j++)
	  if (c)
	    bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp taskloop
    for (i = 0; i < 10; i++)
      if (b)
	#pragma omp parallel for
	for (j = 0; j < 10; j++)
	  if (c)
	    bar ();
	  else
	    baz ();
  else
    bar ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp taskloop simd
    for (i = 0; i < 10; i++)
      if (b)
	bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp for collapse(2)
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
	if (b)
	  bar ();
	else
	  baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp critical
      if (b)
	bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    for (i = 0; i < 10; i++)
      #pragma omp simd
      for (j = 0; j < 10; j++)
	if (b)
	  bar ();
  else
    baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp for simd schedule(runtime)
    for (i = 0; i < 10; i++)
      if (b)
	bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp master
      if (b)
	bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp parallel
      if (b)
	bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    for (i = 0; i < 10; i++)
      #pragma omp parallel for
      for (j = 0; j < 10; j++)
	if (b)
	  bar ();
  else
    baz ();

  if (a) /* { dg-warning "ambiguous" } */
    for (i = 0; i < 10; i++)
      #pragma omp parallel for simd
      for (j = 0; j < 10; j++)
	if (b)
	  bar ();
  else
    baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp single
      if (b)
	bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp task
      if (b)
	bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp taskgroup
      if (b)
	bar ();
      else
	baz ();

  if (a)
    #pragma omp for
    for (i = 0; i < 10; i++)
      {
	if (b)
	  bar ();
	else
	  baz ();
      }

  if (a)
    {
      #pragma omp taskloop
      for (i = 0; i < 10; ++i)
	if (b)
	  bar ();
    }
  else baz ();

  if (a)
    #pragma omp for collapse(2)
    for (i = 0; i < 10; i++)
      {
	for (j = 0; j < 10; j++)
	  if (b)
	    bar ();
	  else
	    baz ();
      }

  if (a)
    #pragma omp critical
      {
	if (b)
	  bar ();
	else
	  baz ();
      }

  if (a)
    for (i = 0; i < 10; i++)
      #pragma omp simd
      for (j = 0; j < 10; j++)
	{
	  if (b)
	    bar ();
	}
  else
    baz ();

  if (a)
    #pragma omp for simd schedule(dynamic, 5)
    for (i = 0; i < 10; i++)
      {
	if (b)
	  bar ();
	else
	  baz ();
      }

  if (a)
    #pragma omp master
      {
	if (b)
	  bar ();
	else
	  baz ();
      }

  if (a)
    #pragma omp parallel
      {
	if (b)
	  bar ();
	else
	  baz ();
      }

  if (a)
    {
      #pragma omp parallel
	if (b)
	  bar ();
	else
	  baz ();
    }

  if (a)
    for (i = 0; i < 10; i++)
      #pragma omp parallel for
      for (j = 0; j < 10; j++)
	{
	  if (b)
	    bar ();
	}
  else
    baz ();

  if (a)
    for (i = 0; i < 10; i++)
      #pragma omp parallel for simd
      for (j = 0; j < 10; j++)
	{
	  if (b)
	    bar ();
	}
  else
    baz ();

  if (a)
    #pragma omp single
      {
	if (b)
	  bar ();
      }
  else
    baz ();

  if (a)
    #pragma omp task
      {
	if (b)
	  bar ();
      }
  else
    baz ();

  if (a)
    #pragma omp taskgroup
      {
	if (b)
	  bar ();
	else
	  baz ();
      }

  if (a)
    #pragma omp taskloop simd
    for (i = 0; i < 10; i++)
      {
	if (b)
	  bar ();
	else
	  baz ();
      }
}

void
f2 (int d, int e, int f)
{
  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp ordered
      if (b)
	bar ();
      else
	baz ();

  if (d) /* { dg-warning "ambiguous" } */
    #pragma omp ordered threads
      if (b)
	bar ();
      else
	baz ();

  if (e)
    #pragma omp ordered
      {
	if (b)
	  bar ();
	else
	  baz ();
      }

  if (f)
    #pragma omp ordered threads
      {
	if (b)
	  bar ();
	else
	  baz ();
      }
}
