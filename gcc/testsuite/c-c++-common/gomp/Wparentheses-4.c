/* PR c/70436 */
/* { dg-additional-options "-Wparentheses -fno-openmp" } */

int a, b, c;
void bar (void);
void baz (void);
void f1 (void);
#pragma omp declare target to (bar, baz, f1, a, b, c)

void
f1 (void)
{
  int i, j;

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp distribute
    for (i = 0; i < 10; i++)
      if (b)
	bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    for (i = 0; i < 10; i++)
      #pragma omp distribute simd
      for (j = 0; j < 10; j++)
	if (b)
	  bar ();
  else
    baz ();

  if (a)
    #pragma omp distribute parallel for
    for (i = 0; i < 10; i++)
      if (b) /* { dg-warning "ambiguous" } */
	#pragma omp parallel for
	for (j = 0; j < 10; j++)
	  if (c)
	    bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp distribute parallel for simd collapse(2)
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
	if (b)
	  bar ();
	else
	  baz ();

  if (a)
    #pragma omp distribute
    for (i = 0; i < 10; i++)
      {
	if (b)
	  bar ();
	else
	  baz ();
      }

  if (a)
    {
      #pragma omp distribute simd
      for (i = 0; i < 10; ++i)
	if (b)
	  bar ();
    }
  else baz ();

  if (a)
    #pragma omp distribute parallel for collapse(2)
    for (i = 0; i < 10; i++)
      {
	for (j = 0; j < 10; j++)
	  if (b)
	    bar ();
	  else
	    baz ();
      }

  if (a)
    for (i = 0; i < 10; i++)
      #pragma omp distribute parallel for simd
      for (j = 0; j < 10; j++)
	{
	  if (b)
	    bar ();
	}
  else
    baz ();
}

void
f2 (void)
{
  int i, j;

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp target teams distribute
    for (i = 0; i < 10; i++)
      if (b)
	bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    for (i = 0; i < 10; i++)
      #pragma omp target teams distribute simd
      for (j = 0; j < 10; j++)
	if (b)
	  bar ();
  else
    baz ();

  if (a)
    #pragma omp target teams distribute parallel for
    for (i = 0; i < 10; i++)
      if (b) /* { dg-warning "ambiguous" } */
	#pragma omp parallel for
	for (j = 0; j < 10; j++)
	  if (c)
	    bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp target teams distribute parallel for simd collapse(2)
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
	if (b)
	  bar ();
	else
	  baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp target teams
      if (b)
	bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp target
    #pragma omp parallel
      if (b)
	bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp target
      if (b)
	bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp target parallel
      if (b)
	bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp target simd
      for (i = 0; i < 10; i++)
	if (b)
	  bar ();
	else
	  baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp target simd collapse(2)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  if (b)
	    bar ();
	  else
	    baz ();

  if (a)
    #pragma omp target teams distribute
    for (i = 0; i < 10; i++)
      {
	if (b)
	  bar ();
	else
	  baz ();
      }

  if (a)
    {
      #pragma omp target teams distribute simd
      for (i = 0; i < 10; ++i)
	if (b)
	  bar ();
    }
  else baz ();

  if (a)
    #pragma omp target teams distribute parallel for collapse(2)
    for (i = 0; i < 10; i++)
      {
	for (j = 0; j < 10; j++)
	  if (b)
	    bar ();
	  else
	    baz ();
      }

  if (a)
    for (i = 0; i < 10; i++)
      #pragma omp target teams distribute parallel for simd
      for (j = 0; j < 10; j++)
	{
	  if (b)
	    bar ();
	}
  else
    baz ();

  if (a)
    #pragma omp target teams
      {
	if (b)
	  bar ();
      }
  else
    baz ();

  if (a)
    #pragma omp target
    #pragma omp parallel
      {
	if (b)
	  bar ();
	else
	  baz ();
      }

  if (a)
    #pragma omp target
      {
	if (b)
	  bar ();
      }
  else
    baz ();

  if (a)
    #pragma omp target parallel
      {
	if (b)
	  bar ();
      }
  else
    baz ();

  if (a)
    #pragma omp target simd
      for (i = 0; i < 10; i++)
	{
	  if (b)
	    bar ();
	  else
	    baz ();
	}

  if (a)
    #pragma omp target simd
      for (i = 0; i < 10; i++)
	{
	  if (b)
	    bar ();
	}
  else
    baz ();

  if (a)
    #pragma omp target simd collapse(2)
      for (i = 0; i < 10; i++)
	{ {
	  for (j = 0; j < 10; j++)
	    if (b)
	      bar ();
	    else
	      baz ();
	} }

  if (a)
    #pragma omp target simd collapse(2)
      for (i = 0; i < 10; i++)
	{ {
	  for (j = 0; j < 10; j++)
	    if (b)
	      bar ();
	  }
	}
  else
    baz ();
}

void
f3 (void)
{
  int i, j;

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp target
    #pragma omp teams distribute
    for (i = 0; i < 10; i++)
      if (b)
	bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    for (i = 0; i < 10; i++)
      #pragma omp target
      #pragma omp teams distribute simd
      for (j = 0; j < 10; j++)
	if (b)
	  bar ();
  else
    baz ();

  if (a)
    #pragma omp target
    #pragma omp teams distribute parallel for
    for (i = 0; i < 10; i++)
      if (b) /* { dg-warning "ambiguous" } */
	#pragma omp parallel for
	for (j = 0; j < 10; j++)
	  if (c)
	    bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp target
    #pragma omp teams distribute parallel for simd collapse(2)
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
	if (b)
	  bar ();
	else
	  baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp target
    #pragma omp teams
      if (b)
	bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp target
    #pragma omp teams
    #pragma omp parallel
      if (b)
	bar ();
      else
	baz ();

  if (a)
    #pragma omp target
    #pragma omp teams distribute
    for (i = 0; i < 10; i++)
      {
	if (b)
	  bar ();
	else
	  baz ();
      }

  if (a)
    {
      #pragma omp target
      #pragma omp teams distribute simd
      for (i = 0; i < 10; ++i)
	if (b)
	  bar ();
    }
  else baz ();

  if (a)
    #pragma omp target
    #pragma omp teams distribute parallel for collapse(2)
    for (i = 0; i < 10; i++)
      {
	for (j = 0; j < 10; j++)
	  if (b)
	    bar ();
	  else
	    baz ();
      }

  if (a)
    for (i = 0; i < 10; i++)
      #pragma omp target
      #pragma omp teams distribute parallel for simd
      for (j = 0; j < 10; j++)
	{
	  if (b)
	    bar ();
	}
  else
    baz ();

  if (a)
    #pragma omp target
    #pragma omp teams
      {
	if (b)
	  bar ();
      }
  else
    baz ();

  if (a)
    #pragma omp target
    #pragma omp teams
    #pragma omp parallel
      {
	if (b)
	  bar ();
	else
	  baz ();
      }
}

void
f4 (void)
{
  if (a) /* { dg-warning "ambiguous" } */
    #pragma omp target data map (tofrom: b)
      if (b)
	bar ();
      else
	baz ();

  if (a)
    #pragma omp target data map (tofrom: b)
      {
	if (b)
	  bar ();
	else
	  baz ();
      }
}
