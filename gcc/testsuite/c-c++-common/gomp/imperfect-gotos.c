/* { dg-do compile } */

/* This file contains tests that are expected to fail.  */


/* These jumps are all OK since they are to/from the same structured block.  */

void f1a (void)
{
#pragma omp for collapse(2)
  for (int i = 0; i < 64; ++i)
    {
      goto a; a:;
      for (int j = 0; j < 64; ++j)
	{
	  goto c; c:;
	}
      goto b; b:;
    }
}

/* Jump around loop body to/from different structured blocks of intervening
   code.  */
void f2a (void)
{
#pragma omp for collapse(2)
  for (int i = 0; i < 64; ++i)
    {
      goto a; a:;
      if (i > 16) goto b; /* { dg-error "invalid branch to/from OpenMP structured block" } */
      for (int j = 0; j < 64; ++j)
	{
	  goto c; c:;
	}
      goto b; b:;
    }
}

/* Jump into loop body from intervening code.  */
void f3a (void)
{
#pragma omp for collapse(2)
  for (int i = 0; i < 64; ++i)
    {
      goto a; a:;
      if (i > 16) goto c; /* { dg-error "invalid branch to/from OpenMP structured block" } */
      for (int j = 0; j < 64; ++j)
	{
	c:
	  ;
	}
      goto b; b:;
    }
}

/* Jump out of loop body to intervening code.  */
void f4a (void)
{
#pragma omp for collapse(2)
  for (int i = 0; i < 64; ++i)
    {
      goto a; a:;
      for (int j = 0; j < 64; ++j)
	if (i > 16) goto c; /* { dg-error "invalid branch to/from OpenMP structured block" } */
      c:
	;
      goto b; b:;
    }
}

/* The next group of tests use the GNU extension for local labels.  Expected
   behavior is the same as the above group.  */

/* These jumps are all OK since they are to/from the same structured block.  */

void f1b (void)
{
#pragma omp for collapse(2)
  for (int i = 0; i < 64; ++i)
    {
      __label__ a, b, c;
      goto a; a:;
      for (int j = 0; j < 64; ++j)
	{
	  goto c; c:;
	}
      goto b; b:;
    }
}

/* Jump around loop body to/from different structured blocks of intervening
   code.  */
void f2b (void)
{
#pragma omp for collapse(2)
  for (int i = 0; i < 64; ++i)
    {
      __label__ a, b, c;
      goto a; a:;
      if (i > 16) goto b; /* { dg-error "invalid branch to/from OpenMP structured block" } */
      for (int j = 0; j < 64; ++j)
	{
	  goto c; c:;
	}
      goto b; b:;
    }
}

/* Jump into loop body from intervening code.  */
void f3b (void)
{
#pragma omp for collapse(2)
  for (int i = 0; i < 64; ++i)
    {
      __label__ a, b, c;
      goto a; a:;
      if (i > 16) goto c; /* { dg-error "invalid branch to/from OpenMP structured block" } */
      for (int j = 0; j < 64; ++j)
	{
	c:
	  ;
	}
      goto b; b:;
    }
}

/* Jump out of loop body to intervening code.  */
void f4b (void)
{
#pragma omp for collapse(2)
  for (int i = 0; i < 64; ++i)
    {
      __label__ a, b, c;
      goto a; a:;
      for (int j = 0; j < 64; ++j)
	if (i > 16) goto c; /* { dg-error "invalid branch to/from OpenMP structured block" } */
      c:
	;
      goto b; b:;
    }
}

/* Test that the even the valid jumps are rejected when intervening code
   is not allowed at all.  */

void f1c (void)
{
#pragma omp for ordered(2)
  for (int i = 0; i < 64; ++i)  /* { dg-error "inner loops must be perfectly nested" } */
    {
      goto a; a:;
      for (int j = 0; j < 64; ++j)
	{
	  goto c; c:;
	}
      goto b; b:;
    }
}

void f1d (void)
{
#pragma omp for ordered(2)
  for (int i = 0; i < 64; ++i)  /* { dg-error "inner loops must be perfectly nested" } */
    {
      __label__ a, b, c;
      goto a; a:;
      for (int j = 0; j < 64; ++j)
	{
	  goto c; c:;
	}
      goto b; b:;
    }
}

