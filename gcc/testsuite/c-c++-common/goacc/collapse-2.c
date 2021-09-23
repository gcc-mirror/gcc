/* Test for ICE as reported in PR98088.  */

int i, j;

void
f1 (void)
{
  #pragma acc parallel
  #pragma acc loop collapse (2)
  for (i = 5; i > 5; i--)
	for (j = 5; j > 0; j--)
	  ;
}

void
f2 (void)
{
  #pragma acc parallel
  #pragma acc loop collapse (2)
  for (i = 0; i < 5; i++)
	for (j = 5; j > 0; j--)
	  ;
}

void
f3 (void)
{
  #pragma acc parallel
  #pragma acc loop collapse (2)
  for (i = 5; i >= 0; i--)
	for (j = 5; j >= 0; j--)
	  ;
}

void f4 ()
{
  #pragma acc parallel loop tile(2, 3)
  for (int i = 0; i > 8; i++)
    for (int j = 0; j > 8; j++);
}

void f5 ()
{
  #pragma acc parallel loop tile(2, 3)
  for (int i = 0; i > 8; i++)
    for (long j = 0; j > 8; j++);
}

void
f6 (int a[32][32])
{
  #pragma acc parallel loop collapse(2)
  for (int i = 16; i > 8; i--)
    for (int j = 16; j > 8; j--)
      a[i][j] = i + j;
}
