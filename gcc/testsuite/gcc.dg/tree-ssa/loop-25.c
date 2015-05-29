/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-profile_estimate" } */

int foo(void);
void bla(void);
void bar(void);

void test1 (void)
{
  unsigned i;

  /* Only one loop should be found here.  */
  i = 0;
  while (1)
    {
      i++;
      if (i == 100)
	break;

      if (foo ())
	bla ();
      else
	bar ();
    }
}

void test2 (void)
{
  unsigned i, j;

  /* Two loops should be found, in this case.  */
  i = j = 0;
  while (1)
    {
      j++;
      foo ();
      if (j < 100)
	continue;

      i++;
      j = 0;
      if (i == 100)
	break;
    }
}

void test3 (void)
{
  unsigned i, j, k;

  /* Three loops.  */
  i = j = k = 0;
  while (1)
    {
      j++;
      foo ();
      if (j < 100)
	continue;

      j = 0;
      k++;
      if (k < 100)
	continue;

      k = 0;
      i++;
      if (i == 100)
	break;
    }
}

void test4 (void)
{
  unsigned i, j, k;

  /* Two loops with a nested subloop.  */
  i = j = 0;
  while (1)
    {
      j++;
      foo ();
      for (k = 0; k < 100; k++)
	foo ();

      if (j < 100)
	continue;

      i++;
      j = 0;
      if (i == 100)
	break;
    }
}


void test5 (void)
{
  unsigned i, j;

  /* Both subloop and non-subloop back edges.  */
  i = j = 0;
  while (1)
    {
      j++;
      foo ();
      if (j < 100)
	continue;
      j = 0;

      i++;
      if (i == 100)
	break;

      if (foo ())
	bla ();
      else
	bar ();
    }
}

/* { dg-final { scan-tree-dump-times "Disambiguating loop" 5 "profile_estimate" } } */
/* For the following xfail marks, see PR35629.  */
/* { dg-final { scan-tree-dump-times "Found latch edge" 5 "profile_estimate" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "Merged latch edges" 2 "profile_estimate" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "4 loops found" 2 "profile_estimate" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "3 loops found" 2 "profile_estimate" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "2 loops found" 1 "profile_estimate" { xfail *-*-* } } } */

