/* { dg-options "-fcondition-coverage -fpath-coverage --coverage" } */
/* { dg-do run } */

int do_something (int x) { return x; }

/*
  Summaries for the same function, with and without suppression.

  Function 'fn1'
  Lines executed:88.89% of 9
  Branches executed:100.00% of 4
  Taken at least once:50.00% of 4
  Calls executed:50.00% of 2
  Condition outcomes covered:50.00% of 4
  Prime paths covered:33.33% of 3

  Function 'fn2'
  Lines executed:83.33% of 6 (3 of 9 suppressed)
  Branches executed:100.00% of 2 (2 of 4 suppressed)
  Taken at least once:50.00% of 2
  Calls executed:0.00% of 1 (1 of 2 suppressed)
  Condition outcomes covered:50.00% of 4
  Prime paths covered:0.00% of 1 (2 of 3 suppressed)
*/

int
fn1 (int argc)
{
  int b = argc + 1;

  int c;

  int a = argc;

  if (a)
    if (b)
      {
      c = do_something (4);
      }
    else
      c = do_something (1024);

  int d = a + c - 1;
}

int
fn2 (int argc)
{
#pragma GCC suppress_coverage begin
  int b = argc + 1;
#pragma GCC suppress_coverage end

#pragma GCC suppress_coverage begin
  int c;
#pragma GCC suppress_coverage end

  int a = argc;

  if (a)
    if (b)
      {
#pragma GCC suppress_coverage begin
      c = do_something (4);
#pragma GCC suppress_coverage end
      }
    else
      c = do_something (1024);

#pragma GCC suppress_coverage begin
  int d = a + c - 1;
#pragma GCC suppress_coverage end
}

/*
  This function has neither conditions nor branches, which means quite
  different summary output.

  Function 'no_branches'
  Lines executed:100.00% of 3
  No branches
  No calls
  No conditions
  Prime paths covered:100.00% of 1
*/
int
no_branches (int a)
{
  int b = a * 2;
  return b;
}

int main ()
{
    fn1 (1);
    fn2 (2);
    no_branches (3);
}

/* { dg-final { run-gcov function-summaries "-fge gcov-43.c" } } */
