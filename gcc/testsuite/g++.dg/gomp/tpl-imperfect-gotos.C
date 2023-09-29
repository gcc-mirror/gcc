/* { dg-do compile } */

/* This file contains tests that are expected to fail.  */


/* These jumps are all OK since they are to/from the same structured block.  */

template<typename T>
void f1a (void)
{
#pragma omp for collapse(2)
  for (T i = 0; i < 64; ++i)
    {
      goto a; a:;
      for (T j = 0; j < 64; ++j)
	{
	  goto c; c:;
	}
      goto b; b:;
    }
}

/* Jump around loop body to/from different structured blocks of intervening
   code.  */
template<typename T>
void f2a (void)
{
#pragma omp for collapse(2)
  for (T i = 0; i < 64; ++i)
    {
      goto a; a:;
      if (i > 16) goto b; /* { dg-error "invalid branch to/from OpenMP structured block" } */
      for (T j = 0; j < 64; ++j)
	{
	  goto c; c:;
	}
      goto b; b:;
    }
}

/* Jump into loop body from intervening code.  */
template<typename T>
void f3a (void)
{
#pragma omp for collapse(2)
  for (T i = 0; i < 64; ++i)
    {
      goto a; a:;
      if (i > 16) goto c; /* { dg-error "invalid branch to/from OpenMP structured block" } */
      for (T j = 0; j < 64; ++j)
	{
	c:  /* { dg-error "jump to label .c." } */
	  ;
	}
      goto b; b:;
    }
}

/* Jump out of loop body to intervening code.  */
template<typename T>
void f4a (void)
{
#pragma omp for collapse(2)
  for (T i = 0; i < 64; ++i)
    {
      goto a; a:;
      for (T j = 0; j < 64; ++j)
	if (i > 16) goto c; /* { dg-error "invalid branch to/from OpenMP structured block" } */
      c:
	;
      goto b; b:;
    }
}

/* The next group of tests use the GNU extension for local labels.  Expected
   behavior is the same as the above group.  */

/* These jumps are all OK since they are to/from the same structured block.  */

template<typename T>
void f1b (void)
{
#pragma omp for collapse(2)
  for (T i = 0; i < 64; ++i)
    {
      __label__ a, b, c;
      goto a; a:;
      for (T j = 0; j < 64; ++j)
	{
	  goto c; c:;
	}
      goto b; b:;
    }
}

/* Jump around loop body to/from different structured blocks of intervening
   code.  */
template<typename T>
void f2b (void)
{
#pragma omp for collapse(2)
  for (T i = 0; i < 64; ++i)
    {
      __label__ a, b, c;
      goto a; a:;
      if (i > 16) goto b; /* { dg-error "invalid branch to/from OpenMP structured block" } */
      for (T j = 0; j < 64; ++j)
	{
	  goto c; c:;
	}
      goto b; b:;
    }
}

/* Jump into loop body from intervening code.  */
template<typename T>
void f3b (void)
{
#pragma omp for collapse(2)
  for (T i = 0; i < 64; ++i)
    {
      __label__ a, b, c;
      goto a; a:;
      if (i > 16) goto c; /* { dg-error "invalid branch to/from OpenMP structured block" } */
      for (T j = 0; j < 64; ++j)
	{
	c:  /* { dg-error "jump to label .c." } */
	  ;
	}
      goto b; b:;
    }
}

/* Jump out of loop body to intervening code.  */
template<typename T>
void f4b (void)
{
#pragma omp for collapse(2)
  for (T i = 0; i < 64; ++i)
    {
      __label__ a, b, c;
      goto a; a:;
      for (T j = 0; j < 64; ++j)
	if (i > 16) goto c; /* { dg-error "invalid branch to/from OpenMP structured block" } */
      c:
	;
      goto b; b:;
    }
}

int main (void)
{
  f1a<int> ();
  f2a<int> ();
  f3a<int> ();
  f4a<int> ();
  f1b<int> ();
  f2b<int> ();
  f3b<int> ();
  f4b<int> ();
}
