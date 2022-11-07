/* Check that execution counts for various C constructs are reported
   correctly by gcov. */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

extern void abort (void);

int do_something (int i)
{
  return i;
}

/* Check static inline functions. */

int unref_val;

static inline int
unreferenced (int i, int j)
{
  return i - j;
}

static inline int
uncalled (int i, int j)
{
  return i * j;
}

static inline int
called (int i, int j)
{
    return i + j;			/* count(1) */
}

void
call_unref ()
{
  if (unref_val)			/* count(1) */
    unref_val = uncalled (1, 2);
  unref_val = called (unref_val, 4);	/* count(1) */
}


/* Check for loops. */

int for_val1;
int for_val2;
int for_temp;

int
test_for1 (int n)
{
  int i;
  for_temp = 1;				/* count(3) */
  for (i = 0; i < n; i++)
    for_temp++;				/* count(9) */
  return for_temp;			/* count(3) */
}

int
test_for2 (int m, int n, int o)
{
  int i, j, k;
  for_temp = 1;				/* count(6) */
  for (i = 0; i < n; i++)
    for (j = 0; j < m; j++)
      for (k = 0; k < o; k++)
	for_temp++;			/* count(81) */
  return for_temp;			/* count(6) */
}

void
call_for ()
{
  for_val1 += test_for1 (0);
  for_val1 += test_for1 (2);
  for_val1 += test_for1 (7);

  for_val2 += test_for2 (0, 0, 0);
  for_val2 += test_for2 (1, 0, 0);
  for_val2 += test_for2 (1, 3, 0);
  for_val2 += test_for2 (1, 3, 1);
  for_val2 += test_for2 (3, 1, 5);
  for_val2 += test_for2 (3, 7, 3);
}

/* Check the use of goto. */

int goto_val;

int
test_goto1 (int f)
{
  if (f)				/* count(2) */
    goto lab1;				/* count(1) */
  return 1;				/* count(1) */
lab1:
  return 2;				/* count(1) */
}

int
test_goto2 (int f)
{
  int i;
  for (i = 0; i < 10; i++)		/* count(15) */
    if (i == f) goto lab2;		/* count(14) */
  return 4;				/* count(1) */
lab2:
  return 8;				/* count(1) */
}

int
test_goto3 (int i, int j)
{
    if (j) goto else_;		/* count(1) */

top:
    if (i)			/* count(1) */
      {
	i = do_something (i);
      }
    else
      {
else_:				/* count(1) */
	j = do_something (j);	/* count(2) */
	if (j)			/* count(2) */
	  {
	    j = 0;		/* count(1) */
	    goto top;		/* count(1) */
	  }
      }
    return 16;
}

void
call_goto ()
{
  goto_val += test_goto1 (0);
  goto_val += test_goto1 (1);
  goto_val += test_goto2 (3);
  goto_val += test_goto2 (30);
  goto_val += test_goto3 (0, 1);
}

/* Check nested if-then-else statements. */

int ifelse_val1;
int ifelse_val2;
int ifelse_val3;

int
test_ifelse1 (int i, int j)
{
  int result = 0;
  if (i)				/* count(5) */
    if (j)				/* count(3) */
      result = do_something (4);	/* count(3) */
    else
      result = do_something (1024);
  else
    if (j)				/* count(2) */
      result = do_something (1);	/* count(1) */
    else
      result = do_something (2);	/* count(1) */
  if (i > j)				/* count(5) */
    result = do_something (result*2);	/* count(1) */
  if (i > 10)				/* count(5) */
    if (j > 10)				/* count(1) */
      result = do_something (result*4);	/* count(1) */
  return result;			/* count(5) */
}

int
test_ifelse2 (int i)
{
  int result = 0;
  if (!i)				/* count(6) */
    result = do_something (1);		/* count(1) */
  if (i == 1)				/* count(6) */
    result = do_something (1024);
  if (i == 2)				/* count(6) */
    result = do_something (2);		/* count(3) */
  if (i == 3)				/* count(6) */
    return do_something (8);		/* count(2) */
  if (i == 4)				/* count(4) */
    return do_something (2048);
  return result;			/* count(4) */
}

int
test_ifelse3 (int i, int j)
{
  int result = 1;
  if (i > 10 && j > i && j < 20)	/* count(11) */
    result = do_something (16);		/* count(1) */
  if (i > 20)				/* count(11) */
    if (j > i)				/* count(5) */
      if (j < 30)			/* count(2) */
	result = do_something (32);	/* count(1) */
  if (i == 3 || j == 47 || i == j)	/* count(11) */
    result = do_something (64);		/* count(3) */
  return result;			/* count(11) */
}

void
call_ifelse ()
{
  ifelse_val1 += test_ifelse1 (0, 2);
  ifelse_val1 += test_ifelse1 (0, 0);
  ifelse_val1 += test_ifelse1 (1, 2);
  ifelse_val1 += test_ifelse1 (10, 2);
  ifelse_val1 += test_ifelse1 (11, 11);

  ifelse_val2 += test_ifelse2 (0);
  ifelse_val2 += test_ifelse2 (2);
  ifelse_val2 += test_ifelse2 (2);
  ifelse_val2 += test_ifelse2 (2);
  ifelse_val2 += test_ifelse2 (3);
  ifelse_val2 += test_ifelse2 (3);

  ifelse_val3 += test_ifelse3 (11, 19);
  ifelse_val3 += test_ifelse3 (25, 27);
  ifelse_val3 += test_ifelse3 (11, 22);
  ifelse_val3 += test_ifelse3 (11, 10);
  ifelse_val3 += test_ifelse3 (21, 32);
  ifelse_val3 += test_ifelse3 (21, 20);
  ifelse_val3 += test_ifelse3 (1, 2);
  ifelse_val3 += test_ifelse3 (32, 31);
  ifelse_val3 += test_ifelse3 (3, 0);
  ifelse_val3 += test_ifelse3 (0, 47);
  ifelse_val3 += test_ifelse3 (65, 65);
}

/* Check switch statements. */

int switch_val, switch_m;

int
test_switch (int i, int j)
{
  int result = 0;			/* count(5) */

  switch (i)				/* count(5) */
    {
      case 1:
        result = do_something (2);	/* count(1) */
        break;				/* count(1) */
      case 2:
        result = do_something (1024);
        break;
      case 3:
      case 4:
        if (j == 2)			/* count(3) */
          return do_something (4);	/* count(1) */
        result = do_something (8);	/* count(2) */
        break;				/* count(2) */
      default:
	result = do_something (32);	/* count(1) */
	switch_m++;			/* count(1) */
        break;
    }
  return result;			/* count(4) */
}

void
call_switch ()
{
  switch_val += test_switch (1, 0);
  switch_val += test_switch (3, 0);
  switch_val += test_switch (3, 2);
  switch_val += test_switch (4, 0);
  switch_val += test_switch (16, 0);	
  switch_val += switch_m;
}

int
main()
{
  call_for ();
  call_goto ();
  call_ifelse ();
  call_switch ();
  call_unref ();
  if ((for_val1 != 12)
      || (for_val2 != 87)
      || (goto_val != 31)
      || (ifelse_val1 != 31)
      || (ifelse_val2 != 23)
      || (ifelse_val3 != 246)
      || (switch_val != 55)
      || (unref_val != 4))
    abort ();
  return 0;
}

/* { dg-final { run-gcov gcov-4.c } } */
