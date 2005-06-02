/* { dg-do run }  */
/* { dg-options -O2 }  */

extern void abort (void);

foo (int i, int j)
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


main()
{
  if (foo (-10, 5) != -2)
    abort ();

  if (foo (-16, 4) != -4)
    abort ();

  if (foo (-15, -5) != 3)
    abort ();

  if (foo (8, 2) != 4)
    abort ();

  if (foo (10, -2) != -5)
    abort ();

  if (foo (20, 5) != 4)
    abort ();

  if (foo (15, -3) != -5)
    abort ();

  return 0;
}
