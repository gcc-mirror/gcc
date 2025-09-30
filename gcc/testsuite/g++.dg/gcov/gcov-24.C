/* { dg-options "--coverage" } */
/* { dg-do run } */

/* testsuite/gcc.misc-tests/gcov-37.c, compiled with the C++ frontend.  */

void noop () {}

int do_something (int i) { return i; }

/* Empty loop bodies should still disable coverage for the for (;;) and compile
   fine.  */
int
empty_body_for_loop ()
{
  int i;
#pragma GCC suppress_coverage begin
  for (i = 0; i < 10; i++)	/* count(#) */
#pragma GCC suppress_coverage end
    ;
  return i;
}

/* Making the for (;;) multi line should report count per line.  g++ considers
   the i++ unexecuted, while gcc counts it.  */
int
ignored_for_loop ()
{
  int i;
#pragma GCC suppress_coverage begin
  for (i = 0; i < 10; i++)	/* count(#) */
    {
      noop ();			/* count(#) */
      noop ();			/* count(#) */
    }
#pragma GCC suppress_coverage end

  /* Making the for (;;) multi line should report count per line.  */
#pragma GCC suppress_coverage begin
  for (i = 0;			/* count(#) */
       i < 20;			/* count(#) */
       i++)			/* count(-) */
    {
      noop ();			/* count(#) */
    }
#pragma GCC suppress_coverage end

  noop ();
  i++;

  return 0;			/* count(1) */
}

int
declarations (int a)
{
  // Should work when it is the first declaration
#pragma GCC suppress_coverage begin
  int b = a + 1;			/* count(#) */
#pragma GCC suppress_coverage end

  // This is ignored in C (no init) but may be implicitly initialized in C++ to
  // an erroneous value, so this could be - (not executed) or # (ignored)
  // depending on the -std= flag.
#pragma GCC suppress_coverage begin
  int c;
#pragma GCC suppress_coverage end

  a *= 2;				/* count(1) */

  // Should work when it is not the first declaration
#pragma GCC suppress_coverage begin
  int d = a - 1;			/* count(#) */
#pragma GCC suppress_coverage end

  c = a+b+d;				/* count(1) */
  return c;
}

int
compound_statements (int a)
{
  int c;
#pragma GCC suppress_coverage begin
  {
    int b = a + 1;			/* count(#) */
    a *= 2;				/* count(#) */
    int d = a - 1;			/* count(#) */
    c = a+b+d;				/* count(#) */
  }
#pragma GCC suppress_coverage end
  return c;				/* count(1) */
}

int
while1 (int a)
{
#pragma GCC suppress_coverage begin
  while (a > 0)			/* count(#) */
    a = do_something (a - 1);		/* count(#) */
#pragma GCC suppress_coverage end
  return a;
}

/* If the pragma stops blocks from merging this would end up with the wrong
   count. The while (cond) should run 6 times (a = 5), but earlier drafts
   clocked it at 7 because an empty block at the end of the body would have
   coverage suppressed, while the loop header did which prevented merging.  */
int
while2 (int a)
{
  while (a > 0)			/* count(6) */
#pragma GCC suppress_coverage begin
    a = do_something (a - 1);		/* count(#) */
#pragma GCC suppress_coverage end
  return a;
}

int
dowhile1 (int a)
{
#pragma GCC suppress_coverage begin
  do
    {
      a = do_something (a - 1);	/* count(#) */
    } while (a > 0);			/* count(#) */
#pragma GCC suppress_coverage end
  return a;
}

int
dowhile2 (int a)
{
  do
    {
#pragma GCC suppress_coverage begin
      a = do_something (a - 1);	/* count(#) */
#pragma GCC suppress_coverage end
    } while (a > 0);			/* count(5) */
  return a;
}

void
call_while ()
{
  while1 (5);
  while2 (5);
  dowhile1 (5);
  dowhile2 (5);
}

/* Based on gcov-pr85217.c, a loop with both breaks and continues.  */
int
for1 ()
{
  int a = 0;
  for (;; a++)		/* count(1) */
    {
      int c[1];
      if (a)		/* count(2) */
	{
	  break;	/* count(1) */
	  a;		/* count(-) */
	  continue; 	/* count(1) */
	}
      continue;	/* count(1) */
    }

  a = 0;
#pragma GCC suppress_coverage begin
  for (;; a++)		/* count(#) */
    {
      int c[1];
      if (a)		/* count(#) */
	{
	  break;	/* count(#) */
	  a;		/* count(-) */
	  continue; 	/* count(#) */
	}
      continue;	/* count(#) */
    }
#pragma GCC suppress_coverage end

  a = 0;
  for (;; a++)		/* count(1) */
    {
      int c[1];
#pragma GCC suppress_coverage begin
      if (a)		/* count(#) */
	{
	  break;	/* count(#) */
	  a;		/* count(-) */
	  continue; 	/* count(#) */
	}
#pragma GCC suppress_coverage end
      continue;	/* count(1) */
    }

  a = 0;
  for (;; a++)		/* count(1) */
    {
      int c[1];
      if (a)		/* count(2) */
	{
#pragma GCC suppress_coverage begin
	  break;	/* count(#) */
#pragma GCC suppress_coverage end
	  a;		/* count(-) */
	  continue; 	/* count(1) */
	}
      continue;	/* count(1) */
    }

  a = 0;
  for (;; a++)		/* count(1) */
    {
      int c[1];
      if (a)		/* count(2) */
	{
	  break;	/* count(1) */
	  a;		/* count(-) */
#pragma GCC suppress_coverage begin
	  continue; 	/* count(#) */
#pragma GCC suppress_coverage end
	}
      continue;	/* count(1) */
    }

  a = 0;
  for (;; a++)		/* count(1) */
    {
      int c[1];
      if (a)		/* count(2) */
	{
	  break;	/* count(1) */
	  a;		/* count(-) */
	  continue; 	/* count(1) */
	}
#pragma GCC suppress_coverage begin
      continue;	/* count(#) */
#pragma GCC suppress_coverage end
    }

  return a;
}

/* A loop with break.  */
int
for2 (int n)
{
  int acc = 0;
  for (int i = 0; i < n; ++i)	/* count(7) */
    {
      acc += do_something (i);	/* count(7) */
#pragma GCC suppress_coverage begin
      if (acc > 10)		/* count(#) */
	break;			/* count(#) */
#pragma GCC suppress_coverage end
      acc -= 1;		/* count(6) */
    }
  return acc;
}

int
for3 (int n)
{
  int acc = 0;
  for (int i = 0; i < n; ++i)	/* count(7) */
    {
      acc += do_something (i);	/* count(7) */
      if (acc > 10)		/* count(7) */
	/* The break is a not-executable-line in C, but will be successfully be
	   ignored in C++.  */
#pragma GCC suppress_coverage begin
	break;			/* count(#) */
#pragma GCC suppress_coverage end
      acc -= 1;		/* count(6) */
    }
  return acc;
}

/* Based on the test in gcov-4.c  */
int for_val1;
int for_temp;
int
nested_for1 (int m, int n, int o)
{
  int i, j, k;
  for_temp = 1;			/* count(6) */
  for (i = 0; i < n; i++)		/* count(20) */
    for (j = 0; j < m; j++)		/* count(44) */
#pragma GCC suppress_coverage begin
      for (k = 0; k < o; k++)		/* count(#) */
	for_temp++;			/* count(#) */
#pragma GCC suppress_coverage end
  return for_temp;			/* count(6) */
}

void
call_for ()
{
  for1 ();
  for2 (10);
  for3 (10);

  for_val1 += nested_for1 (0, 0, 0);
  for_val1 += nested_for1 (1, 0, 0);
  for_val1 += nested_for1 (1, 3, 0);
  for_val1 += nested_for1 (1, 3, 1);
  for_val1 += nested_for1 (3, 1, 5);
  for_val1 += nested_for1 (3, 7, 3);
}

int ifelse_val1;
int ifelse_val2;
int ifelse_val3;

int
test_ifelse1 (int i, int j)
{
  int result = 0;
  /* We can ignore the THEN.  */
  if (i)				/* count(5) */
    if (j)				/* count(3) */
#pragma GCC suppress_coverage begin
      result = do_something (4);	/* count(#) */
    else
#pragma GCC suppress_coverage end
      result = do_something (1024);
  /* We can ignore the ELSE.  */
  else
    if (j)				/* count(2) */
      result = do_something (1);	/* count(1) */
    else
#pragma GCC suppress_coverage begin
      result = do_something (2);	/* count(#) */
#pragma GCC suppress_coverage end
  if (i > j)				/* count(5) */
    result = do_something (result*2);	/* count(1) */

  /* We can ignore the whole if-then-else.  */
  if (i > 10)				/* count(5) */
#pragma GCC suppress_coverage begin
    if (j > 10)			/* count(#) */
      result = do_something (result*4); /* count(#) */
#pragma GCC suppress_coverage end
  return result;			/* count(5) */
}

int
test_ifelse2 (int i)
{
  int result = 0;
#pragma GCC suppress_coverage begin
  if (!i)				/* count(#) */
    result = do_something (1);		/* count(#) */
#pragma GCC suppress_coverage end

  if (i == 1)				/* count(6) */
    result = do_something (1024);

  if (i == 2)				/* count(6) */
#pragma GCC suppress_coverage begin
    result = do_something (2);		/* count(#) */
#pragma GCC suppress_coverage end

  if (i == 3)				/* count(6) */
#pragma GCC suppress_coverage begin
    return do_something (8);		/* count(#) */
#pragma GCC suppress_coverage end

#pragma GCC suppress_coverage begin
  if (i == 4)				/* count(#) */
    return do_something (2048);	/* count(#) */
#pragma GCC suppress_coverage end

  return result;			/* count(4) */
}

int
test_ifelse3 (int i, int j)
{
  int result = 1;
  /* Multi-condition ifs are suppressed, too */
#pragma GCC suppress_coverage begin
  if (i > 10 && j > i && j < 20)	/* count(#) */
    result = do_something (16);	/* count(#) */
#pragma GCC suppress_coverage end

  if (i == 3 || j == 47 || i == j)	/* count(11) */
#pragma GCC suppress_coverage begin
    result = do_something (64);	/* count(#) */
#pragma GCC suppress_coverage end

  return result;			/* count(11) */
}

/* These are based on gcov-17.c  */
int
test_ifelse4 (int true_var, int false_var)
{
  unsigned int ret = 0;
#pragma GCC suppress_coverage begin
  if (true_var)		/* count(#) */
    {
      if (false_var)		/* count(#) */
	ret = 111;		/* count(#) */
    }
  else
    ret = 999;			/* count(#) */
#pragma GCC suppress_coverage end
  return ret;
}

int
test_ifelse5 (int true_var, int false_var)
{
  unsigned int ret = 0;
  if (true_var)		/* count(1) */
#pragma GCC suppress_coverage begin
    {
      if (false_var)		/* count(#) */
	ret = 111;		/* count(#) */
#pragma GCC suppress_coverage end
    }
  else
    ret = 999;			/* count(#####) */
  return ret;
}

int
test_ifelse6 (int true_var, int false_var)
{
  unsigned int ret = 0;
  if (true_var)		/* count(1) */
    {
#pragma GCC suppress_coverage begin
      if (false_var)		/* count(#) */
	ret = 111;		/* count(#) */
#pragma GCC suppress_coverage end
    }
  else
    ret = 999;			/* count(#####) */
  return ret;
}

int
test_ifelse7 (int true_var, int false_var)
{
  unsigned int ret = 0;
  if (true_var)		/* count(1) */
    {
      if (false_var)		/* count(1) */
#pragma GCC suppress_coverage begin
	ret = 111;		/* count(#) */
#pragma GCC suppress_coverage end
    }
  else
    ret = 999;			/* count(#####) */
  return ret;
}

int
test_ifelse8 (int true_var, int false_var)
{
  unsigned int ret = 0;
  if (true_var)		/* count(1) */
    {
      if (false_var)		/* count(1) */
	ret = 111;		/* count(#####) */
    }
  else
#pragma GCC suppress_coverage begin
    ret = 999;			/* count(#) */
#pragma GCC suppress_coverage end
  return ret;
}

int
test_ifelse9 (int true_var, int false_var)
{
  unsigned int ret = 0;
  /* With the pragma we can disable condition, but still observe the then-else.  */
#pragma GCC suppress_coverage begin
  if (true_var)		/* count(#) */
#pragma GCC suppress_coverage end
    {
      if (false_var)		/* count(1) */
	ret = 111;		/* count(#####) */
    }
  else
    ret = 999;			/* count(#####) */
  return ret;
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

  test_ifelse4 (1, 0);
  test_ifelse5 (1, 0);
  test_ifelse6 (1, 0);
  test_ifelse7 (1, 0);
  test_ifelse8 (1, 0);
  test_ifelse9 (1, 0);
}

int switch_val, switch_m;
int
test_switch (int i, int j)
{
  int result = 0;			/* count(5) */
  /* We can disable individual statements and breaks in the switch.  */
  switch (i)				/* count(5) */
    {
    case 1:
#pragma GCC suppress_coverage begin
      result = do_something (2);	/* count(#) */
#pragma GCC suppress_coverage end
      break;				/* count(1) */
    case 2:
      result = do_something (1024);
      break;
    case 3:
    case 4:
      if (j == 2)			/* count(3) */
	return do_something (4);	/* count(1) */
      result = do_something (8);	/* count(2) */
#pragma GCC suppress_coverage begin
      break;				/* count(#) */
#pragma GCC suppress_coverage end
    default:
      result = do_something (32);	/* count(1) */
#pragma GCC suppress_coverage begin
      switch_m++;			/* count(#) */
#pragma GCC suppress_coverage end
      break;
    }

  /* We can disable the whole switch.  */
#pragma GCC suppress_coverage begin
  switch (i)
    {
    case 1:
      result = do_something (64);	/* count(#) */
      break;				/* count(#) */
    case 2:
      result = do_something (128);	/* count(#) */
      break;				/* count(#) */
    case 3:
      result = do_something (256);	/* count(#) */
      break;				/* count(#) */
    default:
      result = do_something (512);	/* count(#) */
      switch_m++;			/* count(#) */
      break;
    }
#pragma GCC suppress_coverage end

  return result;			/* count(4) */
}

int
test_switch2 (int i, int j)
{
  int result = 0;			/* count(1) */
  switch (i)				/* count(1) */
    {
    case 1:
      result = do_something (2);	/* count(#####) */
      break;				/* count(#####) */
    case 2:
      result = do_something (1024);
      break;
    case 3:
    case 4:
      if (j == 2)			/* count(#####) */
	return do_something (4);	/* count(#####) */
      result = do_something (8);	/* count(#####) */
      break;				/* count(#####) */
      /* We can disable the label itself with the pragma.  */
#pragma GCC suppress_coverage begin
    default:				/* count(#) */
      result = do_something (32);	/* count(#) */
      switch_m++;			/* count(#) */
      break;
#pragma GCC suppress_coverage end
    }

  /* We can disable multiple cases with a single pragma.  */
#pragma GCC suppress_coverage begin
  switch (i)				/* count(#) */
    {
    case 1:				/* count(#) */
      result = do_something (64);	/* count(#) */
      break;				/* count(#) */
    case 2:
      result = do_something (128);	/* count(#) */
      break;				/* count(#) */
#pragma GCC suppress_coverage end
    case 3:
      result = do_something (256);	/* count(#####) */
      break;				/* count(#####) */
    default:
      result = do_something (512);	/* count(1) */
      switch_m++;			/* count(1) */
      break;
    }

  return result;			/* count(1) */
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
  switch_val += test_switch2 (16, 0);
}

/* The goto tests from gcov-4.c.  */
int goto_val;

int
test_goto1 (int f)
{
#pragma GCC suppress_coverage begin
  if (f)				/* count(#) */
    goto lab1;				/* count(#) */
#pragma GCC suppress_coverage end
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
  // Add an empty statement so the attribute is applied to the return, not the
  // label.
  ;
#pragma GCC suppress_coverage begin
  return 8;				/* count(#) */
#pragma GCC suppress_coverage end
}

int
test_goto3 (int i, int j)
{
  if (j)			/* count(1) */
#pragma GCC suppress_coverage begin
    goto else_;		/* count(#) */
#pragma GCC suppress_coverage end

top:
    if (i)			/* count(1) */
      {
	i = do_something (i);
      }
    else
      {
else_:				/* count(1) */
	j = do_something (j);	/* count(2) */
#pragma GCC suppress_coverage begin
	if (j)			/* count(#) */
	  {
	    j = 0;		/* count(#) */
	    goto top;		/* count(#) */
	  }
#pragma GCC suppress_coverage end
      }
    return 16;			/* count(1) */
}

/* Not from gcov-4.c  */
int
test_goto4 (int f, int g)
{
  /* The attribute should apply to all statements inside the {}, even the goto
     when the label is inside the suppressed block.  When jumping out, the
     destination should still be counted.  */
#pragma GCC suppress_coverage begin
  {
    if (f)			/* count(#) */
      goto inside;		/* count(#) */
    if (g)			/* count(#) */
      goto outside;		/* count(#) */

  inside:
    if (g)			/* count(#) */
      goto skip;		/* count(#) */
    f += 2;			/* count(#) */

  skip:
    g += 2;			/* count(#) */
  }
#pragma GCC suppress_coverage end
  return 1;			/* count(3) */
outside:
  return 2;			/* count(1) */
}

/* Based on gcov-18.c  */
int
test_goto5 (int a)
{
  /* If just one statement is ignored, the whole line should be.  */
#pragma GCC suppress_coverage begin
  noop (); goto baz; lab: a = do_something (a+1); /* count(#) */
#pragma GCC suppress_coverage end
 baz:
  if (a == 1) /* count(2) */
    goto lab; /* count(1) */
  return a;
}

int
test_goto6 (int a)
{
#pragma GCC suppress_coverage begin
  {
    a += 1; /* count(#) */
    if (a >= 2) /* count(#) */
      goto goto5_1; /* count(#) */

    a += 10; /* count(#) */
    if (a >= 20) /* count(#) */
      goto goto5_2; /* count(#) */


  goto5_1: /* count(#) */
    a *= 3;  /* count(#) */
    goto goto5_2; /* count(#) */

  goto5_2: /* count(#) */
    a -= 2; /* count(#) */

  goto5_3: /* count(#) */
    a += 4; /* count(#) */
    goto goto5_after; /* count(#) */
  }
#pragma GCC suppress_coverage end
  a *= 2; /* count(-) */

 goto5_after: /* count(1) */
  a -= 1; /* count(1) */
  return a; /* count(1) */
}

void
call_goto ()
{
  goto_val += test_goto1 (0);
  goto_val += test_goto1 (1);
  goto_val += test_goto2 (3);
  goto_val += test_goto2 (30);
  goto_val += test_goto3 (0, 1);

  goto_val += test_goto4 (0, 0);
  goto_val += test_goto4 (0, 1);
  goto_val += test_goto4 (1, 0);
  goto_val += test_goto4 (1, 1);

  goto_val += test_goto5 (1);
  goto_val += test_goto6 (1);
}

/* Returns, guarded by both plain values and function calls.

   The report is slightly different between C and C++ and the #/- are anchored
   to the right line.  */
void
return1 (int a, int b, int c)
{
#pragma GCC suppress_coverage begin
    if (a) return;				/* count(#) */
#pragma GCC suppress_coverage end
#pragma GCC suppress_coverage begin
  if (do_something (b)) return;		/* count(#) */
#pragma GCC suppress_coverage end
  if (do_something (c))			/* count(1) */
#pragma GCC suppress_coverage begin		/* count(-) */
    return;					/* count(#) */
#pragma GCC suppress_coverage end
  if (do_something (c)) {			/* count(#####) */
#pragma GCC suppress_coverage begin		/* count(-) */
    return;					/* count(#) */
#pragma GCC suppress_coverage end
  }
}

int
return2 (int a, int b, int c)
{
#pragma GCC suppress_coverage begin
    if (a) return a;				/* count(#) */
#pragma GCC suppress_coverage end		/* count(-) */
#pragma GCC suppress_coverage begin		/* count(-) */
  if (do_something (b)) return b;		/* count(#) */
#pragma GCC suppress_coverage end		/* count(-) */
  if (do_something (c))			/* count(1) */
#pragma GCC suppress_coverage begin		/* count(-) */
    return c;					/* count(#) */
#pragma GCC suppress_coverage end		/* count(-) */
  return 0;
}

void
call_return ()
{
  return1 (1, 0, 0);
  return1 (0, 1, 0);
  return1 (0, 0, 1);
  return2 (1, 0, 0);
  return2 (0, 1, 0);
  return2 (0, 0, 1);
}

/* From gcov-6.c  */
extern "C" void exit (int);
int test_exit_val;

void
test_exit1 (int i)
{
  /* An abnormal exit should not break suppression.  */
#pragma GCC suppress_coverage begin
  if (i < 0)			/* count(#) */
    exit (0);			/* count(#) */
#pragma GCC suppress_coverage end
  test_exit_val += i;		/* count(3) */
}

void
test_exit2 (int i)
{
  if (i < 0)			/* count(4) */
#pragma GCC suppress_coverage begin
    exit (0);			/* count(#) */
#pragma GCC suppress_coverage end
  test_exit_val += i;		/* count(3) */
}

void
test_exit3 (int i)
{
  /* There can be statements on either side of exit ().  */
#pragma GCC suppress_coverage begin
  if (i < 0)			/* count(#) */
    {
      test_exit_val += i;	/* count(#) */
      exit (0);		/* count(#) */
      test_exit_val += i;	/* count(-) */
    }
#pragma GCC suppress_coverage end
  test_exit_val += i;		/* count(3) */
}

void
call_exit ()
{
  for (int i = 0; i != 3; ++i)
    test_exit1 (i);
  for (int i = 0; i != 3; ++i)
    test_exit2 (i);
  for (int i = 0; i != 3; ++i)
    test_exit3 (i);

  test_exit2 (-1);
}

int
computed_goto1 (int a)
{
  void *op;
#pragma GCC suppress_coverage begin
  op = &&dest;		/* count(#) */
#pragma GCC suppress_coverage end
dest:
  if (op && a > 0)	/* count(6) */
    {
      a -= 1;		/* count(5) */
      goto *op;	/* count(5) */
    }

  return a;
}

int
computed_goto2 (int a)
{
  void *op = &&dest;	/* count(1) */
dest:
  ;
#pragma GCC suppress_coverage begin
  if (op && a > 0)	/* count(#) */
    {
      a -= 1;		/* count(#) */
      goto *op;	/* count(#) */
    }
#pragma GCC suppress_coverage end

  return a;
}

int
computed_goto3 (int a)
{
  void *op = &&dest;	/* count(1) */
dest:
  ;
  if (op && a > 0)	/* count(6) */
    {
      a -= 1;		/* count(5) */
#pragma GCC suppress_coverage begin
      goto *op;	/* count(#) */
#pragma GCC suppress_coverage end
    }

  return a;
}

void
call_computed_goto ()
{
  computed_goto1 (5);
  computed_goto2 (5);
  computed_goto3 (5);
}

__attribute__((suppress_coverage)) int
suppressed_function (int a)
{
  int c;
  int b = a + 1;		/* count(#) */
  a *= 2;			/* count(#) */
  int d = a - 1;		/* count(#) */
  c = a+b+d;			/* count(#) */
  return c;			/* count(#) */
}

#pragma GCC suppress_coverage begin
int
pragma_begin_outside (int a)
{
  int c;
  int b = a + 1;		/* count(#) */
  a *= 2;			/* count(#) */
  int d = a - 1;		/* count(#) */
  c = a+b+d;			/* count(#) */
  return c;			/* count(#) */
}
#pragma GCC suppress_coverage end

#pragma GCC suppress_coverage begin
int
pragma_end_middle (int a)
{
  int c;
  int b = a + 1;		/* count(#) */
  a *= 2;			/* count(#) */
#pragma GCC suppress_coverage end
  int d = a - 1;		/* count(1) */
  c = a+b+d;			/* count(1) */
  return c;			/* count(1) */
}

int main ()
{
  empty_body_for_loop ();
  ignored_for_loop ();
  declarations (1);
  compound_statements (1);
  call_while ();
  call_for ();
  call_ifelse ();
  call_switch ();
  call_goto ();
  call_return ();
  call_computed_goto ();
  suppressed_function (1);
  pragma_begin_outside (1);
  pragma_end_middle (1);

  /* The final test will actually exit, so make sure to call it last.  */
  call_exit ();
  return 0;
}

/* { dg-final { run-gcov { gcov-24.C } } } */
