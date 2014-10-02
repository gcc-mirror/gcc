/* { dg-do run }  */
/* { dg-options -O2 }  */

extern void abort (void);
extern void link_error (void);

int
foo_div (int i, int j)
{
  int k;

  /* [-INF, -1] / [1, +INF] should not give [-1, -1].  */
  if (i <= -1)
    if (j >= 1)
      {
	k = i / j;
	if (k == -1)
	  abort ();

	return k;
      }

  /* [-20, -10] / [2, 10] should give [-10, -1].  */
  if (i >= -20)
    if (i <= -10)
      if (j >= 2)
	if (j <= 10)
	  {
	    k = i / j;
	    if (k < -10)
	      link_error ();
	    if (k > -1)
	      link_error ();

	    return k;
	  }

  /* [-20, -10] / [-10, -2] should give [1, 10].  */
  if (i >= -20)
    if (i <= -10)
      if (j >= -10)
	if (j <= -2)
	  {
	    k = i / j;
	    if (k < 1)
	      link_error ();
	    if (k > 10)
	      link_error ();

	    return k;
	  }

  /* [-20, 10] / [2, 10] should give [-10, 5].  */
  if (i >= -20)
    if (i <= 10)
      if (j >= 2)
	if (j <= 10)
	  {
	    k = i / j;
	    if (k < -10)
	      link_error ();
	    if (k > 5)
	      link_error ();

	    return k;
	  }

  /* [-20, 10] / [-10, -2] should give [-5, 10].  */
  if (i >= -20)
    if (i <= 10)
      if (j >= -10)
	if (j <= -2)
	  {
	    k = i / j;
	    if (k < -5)
	      link_error ();
	    if (k > 10)
	      link_error ();

	    return k;
	  }

  /* [10, 20] / [2, 10] should give [1, 10].  */
  if (i >= 10)
    if (i <= 20)
      if (j >= 2)
	if (j <= 10)
	  {
	    k = i / j;
	    if (k < 1)
	      link_error ();
	    if (k > 10)
	      link_error ();

	    return k;
	  }

  /* [10, 20] / [-10, -2] should give [-10, -1].  */
  if (i >= 10)
    if (i <= 20)
      if (j >= -10)
	if (j <= -2)
	  {
	    k = i / j;
	    if (k > -1)
	      link_error ();
	    if (k < -10)
	      link_error ();

	    return k;
	  }

  abort ();
}

int
foo_mult (int i, int j)
{
  int k;

  /* [-20, -10] * [2, 10] should give [-200, -20].  */
  if (i >= -20)
    if (i <= -10)
      if (j >= 2)
	if (j <= 10)
	  {
	    k = i * j;
	    if (k < -200)
	      link_error ();
	    if (k > -20)
	      link_error ();

	    return k;
	  }

  /* [-20, -10] * [-10, -2] should give [20, 200].  */
  if (i >= -20)
    if (i <= -10)
      if (j >= -10)
	if (j <= -2)
	  {
	    k = i * j;
	    if (k < 20)
	      link_error ();
	    if (k > 200)
	      link_error ();

	    return k;
	  }

  /* [-20, 10] * [2, 10] should give [-200, 100].  */
  if (i >= -20)
    if (i <= 10)
      if (j >= 2)
	if (j <= 10)
	  {
	    k = i * j;
	    if (k < -200)
	      link_error ();
	    if (k > 100)
	      link_error ();

	    return k;
	  }

  /* [-20, 10] * [-10, -2] should give [-100, 200].  */
  if (i >= -20)
    if (i <= 10)
      if (j >= -10)
	if (j <= -2)
	  {
	    k = i * j;
	    if (k < -100)
	      link_error ();
	    if (k > 200)
	      link_error ();

	    return k;
	  }

  /* [10, 20] * [2, 10] should give [20, 200].  */
  if (i >= 10)
    if (i <= 20)
      if (j >= 2)
	if (j <= 10)
	  {
	    k = i * j;
	    if (k < 20)
	      link_error ();
	    if (k > 200)
	      link_error ();

	    return k;
	  }

  /* [10, 20] * [-10, -2] should give [-200, -20].  */
  if (i >= 10)
    if (i <= 20)
      if (j >= -10)
	if (j <= -2)
	  {
	    k = i * j;
	    if (k < -200)
	      link_error ();
	    if (k > -20)
	      link_error ();

	    return k;
	  }

  abort ();
}

int
main()
{
  if (foo_div (-10, 5) != -2)
    abort ();

  if (foo_div (-16, 4) != -4)
    abort ();

  if (foo_div (-15, -5) != 3)
    abort ();

  if (foo_div (8, 2) != 4)
    abort ();

  if (foo_div (10, -2) != -5)
    abort ();

  if (foo_div (20, 5) != 4)
    abort ();

  if (foo_div (15, -3) != -5)
    abort ();

  if (foo_mult (-10, 5) != -50)
    abort ();

  if (foo_mult (-16, 4) != -64)
    abort ();

  if (foo_mult (-15, -5) != 75)
    abort ();

  if (foo_mult (8, 2) != 16)
    abort ();

  if (foo_mult (10, -2) != -20)
    abort ();

  if (foo_mult (20, 5) != 100)
    abort ();

  if (foo_mult (15, -3) != -45)
    abort ();

  return 0;
}
