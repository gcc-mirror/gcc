/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fdump-ipa-cp -fno-early-inlining"  } */
/* { dg-add-options bind_pic_locally } */

extern int get_stuff (int);
extern void do_stuff (int);
extern void do_stuff2 (int);
extern void do_other_stuff (void);
extern int get_element (int, int, int);
extern int adjust (int, int, int, int);

extern int count;

int
foo (int s, int p)
{
  int c, r = 0;

  for (c = 0 ; c < count; c++)
    {
      r += get_stuff (s);
      /* The following is just something big that can go away.  */
      if (p != 0)
	{
	  int a[64][64];
	  int i, j, k;

	  for (i = 0; i < 64; i++)
	    for (j = 0; j < 64; j++)
	      a[i][j] = get_element (p + c, i, j);

	  for (k = 0; k < 4; k++)
	    {
	      r = r / 2;

	      for (i = 1; i < 63; i++)
		for (j = 62; j > 0; j--)
		  a[i][j] += adjust (a[i-1][j], a[i][j-1],
				     a[i+1][j], a[i][j+1]);

	      for (i = 4; i < 64; i += 4)
		for (j = 4; j < 64; j += 4)
		  r += a[i][j] / 4;
	    }
	}
    }
  return r;
}

int
bar (int p, int q)
{
  if (q > 0)
    do_stuff (q);
  else
    do_stuff (-q);

  if (q % 2)
    do_stuff2 (2 * q);
  else
    do_stuff2 (2 * (q + 1));

  return foo (4, p);
}

int
bah (int p, int q)
{
  int i, j;

  while (q < -20)
    q += get_stuff (-q);

  for (i = 0; i < 36; i++)
    for (j = 0; j < 36; j++)
      do_stuff (get_stuff (q * i + 2));

  bar (p, q);
}

int
top1 (int q)
{
  do_other_stuff ();
  return bah (0, q);
}

int
top2 (int q)
{
  do_stuff (200);
  do_other_stuff ();
  return bah (16, q);
}

/* { dg-final { scan-ipa-dump-times "Creating a specialized node of foo" 1 "cp" } } */
/* { dg-final { scan-ipa-dump-times "replacing param .. p with const 0" 3 "cp"  } } */
/* { dg-final { scan-ipa-dump "replacing param .0 s with const 4" "cp"  } } */
