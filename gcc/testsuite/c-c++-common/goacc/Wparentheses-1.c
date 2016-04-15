/* PR c/70436 */
/* { dg-additional-options "-Wparentheses" } */

int a, b, c;
void bar (void);
void baz (void);
#pragma acc routine
void bar2 (void);
#pragma acc routine
void baz2 (void);

void
f1 (void)
{
  int i, d[10] = { 0 };

  if (a) /* { dg-warning "ambiguous" } */
    #pragma acc data copyin (d[0:10])
      if (b)
	bar ();
      else
	baz ();

  #pragma acc data copyin (d[0:10])
  if (a) /* { dg-warning "ambiguous" } */
    #pragma acc host_data use_device (d)
      if (b)
	bar ();
      else
	baz ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma acc kernels
      if (b)
	bar2 ();
      else
	baz2 ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma acc kernels
    for (i = 0; i < 10; i++)
      if (b)
	bar2 ();
      else
	baz2 ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma acc parallel
      if (b)
	bar2 ();
      else
	baz2 ();

  if (a) /* { dg-warning "ambiguous" } */
    #pragma acc parallel loop
    for (i = 0; i < 10; i++)
      if (b)
	bar2 ();
      else
	baz2 ();

  (void) d[0];

  if (a)
    #pragma acc data copyin (d[0:10])
      {
	if (b)
	  bar ();
	else
	  baz ();
      }

  if (a)
    #pragma acc data copyin (d[0:10])
      {
	if (b)
	  bar ();
      }
  else
    baz ();

  #pragma acc data copyin (d[0:10])
  if (a)
    #pragma acc host_data use_device (d)
    {
      if (b)
	bar ();
      else
	baz ();
    }

  #pragma acc data copyin (d[0:10])
  if (a)
    #pragma acc host_data use_device (d)
    {
      if (b)
	bar ();
    }
  else
    baz ();

  if (a)
    #pragma acc kernels
    {
      if (b)
	bar2 ();
      else
	baz2 ();
    }

  if (a)
    #pragma acc kernels
    for (i = 0; i < 10; i++)
      {
	if (b)
	  bar2 ();
	else
	  baz2 ();
      }

  if (a)
    #pragma acc parallel
      {
	if (b)
	  bar2 ();
	else
	  baz2 ();
      }

  if (a)
    #pragma acc parallel loop
    for (i = 0; i < 10; i++)
      {
	if (b)
	  bar2 ();
	else
	  baz2 ();
      }

  if (a)
    {
      #pragma acc parallel loop
      for (i = 0; i < 10; i++)
	if (b)
	  bar2 ();
	else
	  baz2 ();
    }
}

#pragma acc routine vector
void
f2 (int *a, int b, int c)
{
  int i;

  if (b) /* { dg-warning "ambiguous" } */
    #pragma acc loop vector
      for (i = 0; i < 10; i++)
	if (c)
	  a[i] = a[i] + 1;
	else
	  a[i] = a[i] + 2;

  if (b)
    #pragma acc loop vector
      for (i = 0; i < 10; i++)
	{
	  if (c)
	    a[i] = a[i] + 1;
	  else
	    a[i] = a[i] + 2;
	}
}
