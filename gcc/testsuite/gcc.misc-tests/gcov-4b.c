/* Check that execution counts for various C constructs are reported
   correctly by gcov. */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

extern void abort (void);

int do_something (int i)
{
  return i;
}

/* Check for loops. */

int for_val1;
int for_val2;
int for_temp;

int
test_for1 (int n)
{
  int i;
  for_temp = 1;
  for (i = 0; i < n; i++)		/* branch(25) */
    					/* branch(end) */
    for_temp++;
  return for_temp;
}

int
test_for2 (int m, int n, int o)
{
  int i, j, k;
  for_temp = 1;
  for (i = 0; i < n; i++)		/* branch(30) */
    					/* branch(end) */
    for (j = 0; j < m; j++)		/* branch(32) */
    					/* branch(end) */
      for (k = 0; k < o; k++)		/* branch(27) */
    					/* branch(end) */
	for_temp++;
  return for_temp;
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
  if (f)				/* branch(50) */
    					/* branch(end) */
    goto lab1;
  return 1;
lab1:
  return 2;
}

int
test_goto2 (int f)
{
  int i;
  for (i = 0; i < 10; i++)		/* branch(7) */
    					/* branch(end) */
    if (i == f) goto lab2;
  return 4;
lab2:
  return 8;
}

void
call_goto ()
{
  goto_val += test_goto1 (0);
  goto_val += test_goto1 (1);
  goto_val += test_goto2 (3);
  goto_val += test_goto2 (30);
}

/* Check nested if-then-else statements. */

int ifelse_val1;
int ifelse_val2;
int ifelse_val3;

int
test_ifelse1 (int i, int j)
{
  int result = 0;
  if (i)				/* branch(40) */
    					/* branch(end) */
    if (j)				/* branch(0) */
    					/* branch(end) */
      result = do_something (4);
    else
      result = do_something (1024);
  else
    if (j)				/* branch(50) */
    					/* branch(end) */
      result = do_something (1);
    else
      result = do_something (2);
  if (i > j)				/* branch(80) */
    					/* branch(end) */
    result = do_something (result*2);
  if (i > 10)				/* branch(80) */
    					/* branch(end) */
    if (j > 10)				/* branch(100) */
      result = do_something (result*4);
  return result;
}

int
test_ifelse2 (int i)
{
  int result = 0;
  if (!i)				/* branch(83) */
    					/* branch(end) */
    result = do_something (1);
  if (i == 1)				/* branch(100) */
    					/* branch(end) */
    result = do_something (1024);
  if (i == 2)				/* branch(50) */
    					/* branch(end) */
    result = do_something (2);
  if (i == 3)				/* branch(67) */
    					/* branch(end) */
    return do_something (8);
  if (i == 4)				/* branch(100) */
    					/* branch(end) */
    return do_something (2048);
  return result;
}

int
test_ifelse3 (int i, int j)
{
  int result = 1;
  if (i > 10 && j > i && j < 20)	/* branch(27 50 75) */
    					/* branch(end) */
    result = do_something (16);
  if (i > 20)				/* branch(55) */
    					/* branch(end) */
    if (j > i)				/* branch(60) */
    					/* branch(end) */
      if (j < 30)			/* branch(50) */
    					/* branch(end) */
	result = do_something (32);
  if (i == 3 || j == 47 || i == j)	/* branch(9 10 89) */
    					/* branch(end) */
    result = do_something (64);
  return result;
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
  int result = 0;

  switch (i)				/* branch(80 25) */
    					/* branch(end) */
    {
      case 1:
        result = do_something (2);
        break;
      case 2:
        result = do_something (1024);
        break;
      case 3:
      case 4:
        if (j == 2)			/* branch(67) */
    					/* branch(end) */
          return do_something (4);
        result = do_something (8);
        break;
      default:
	result = do_something (32);
	switch_m++;
        break;
    }
  return result;
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
  if ((for_val1 != 12)
      || (for_val2 != 87)
      || (goto_val != 15)
      || (ifelse_val1 != 31)
      || (ifelse_val2 != 23)
      || (ifelse_val3 != 246)
      || (switch_val != 55))
    abort ();
  return 0;
}

/* { dg-final { run-gcov branches { -b gcov-4b.c } } } */
