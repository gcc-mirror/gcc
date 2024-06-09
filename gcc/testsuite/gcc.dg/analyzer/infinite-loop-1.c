/* { dg-additional-options "-O0" } */

extern int maybe_useful_work ();

int global_var;
volatile int volatile_global_var;

struct st
{
  int x;
  int y;
};

static void __attribute__((noinline))
do_nothing (void)
{
}

void test_empty_while_true ()
{
  while (1) {} /* { dg-warning "infinite loop" } */
  /* { dg-message "looping back\.\.\." "" { target *-*-* } .-1 } */
  /* { dg-message "\.\.\.to here" "" { target *-*-* } .-2 } */
}

void test_empty_do_while ()
{
  do {} while (1);  /* { dg-warning "infinite loop" } */
  /* { dg-message "looping back\.\.\." "" { target *-*-* } .-1 } */
  /* { dg-message "\.\.\.to here" "" { target *-*-* } .-2 } */
}

void test_empty_for ()
{
  for (;;) {} /* { dg-warning "infinite loop" } */
  /* { dg-message "looping back\.\.\." "" { target *-*-* } .-1 } */
  /* { dg-message "\.\.\.to here" "" { target *-*-* } .-2 } */
}

void test_while_true_maybe_useful_work ()
{
  while (1)
    maybe_useful_work ();
}

void test_while_true_interproc_empty ()
{
  /* Depending on optimization level, location is sometimes
     on "while", sometimes on "do_nothing", and sometimes the
     unknown location.  */
  while (1)
    do_nothing ();  /* { dg-warning "infinite loop" } */
}

void test_while_true_increment_local ()
{
  int i = 0;
  while (1)
    i++;  /* { dg-warning "infinite loop" } */
}

void test_while_true_increment_global ()
{
  while (1)
    global_var++;
}

void test_guarded_while_true_increment_local (int flag)
{
  if (flag)
    {
      int i = 0;
      while (1)
	i++;  /* { dg-warning "infinite loop" } */
    }
}

void test_while_local_flag_increment_local (int flag)
{
  int i = 0;
  while (flag) /* { dg-warning "infinite loop" } */
    i++;
}

extern int check_flag (void);

void test_while_calling_fn (void)
{
  while (check_flag ())
    do_nothing ();
}

void test_missing_parens_on_call (void)
{
  while (check_flag)
    do_nothing ();  /* { dg-warning "infinite loop" } */
}

void test_iteration_copy_and_paste_error (int m, int n)
{
  /* Wrong variable is incremented in inner "for" loop, thus
     effectively an infinite loop.  */
  float arr[m][n];
  for (int i = 0; i < m; i++)
    for (int j = 0; j < n; i++) /* { dg-warning "infinite loop" } */
      arr[i][j] = 0.f;
}

void test_missing_comparison_in_for_condition_1 (int n)
{
  /* Should have been "i < n", rather than just "n".  */
  for (int i = 0; n; i++) /* { dg-warning "infinite loop" } */
    {
    }
}

int test_missing_comparison_in_for_condition_2 (int val, int *arr, int n)
{
  /* Should have been "i < n", rather than just "n".  */
  int acc = 0;
  for (int i = 0; n; i++) /* { dg-warning "infinite loop" } */
    acc += arr[i];
  return acc;
}

void test_non_volatile_local_1 (void)
{
  int flag = 0;
  while (!flag) /* { dg-warning "infinite loop" } */
    {
    }
}

void test_non_volatile_local_2a (void)
{
  int flag = 0;

  /* Perhaps should complain about this.
     Although the infinite loop might be doing useful work,
     "while (!flag)" is a misleading way to spell "infinite loop".  */
  while (!flag)
    maybe_useful_work ();
}

void test_non_volatile_local_2b (void)
{
  int flag = 0;

  while (!flag)
    flag = maybe_useful_work ();
}

void test_non_volatile_local_3a (int n)
{
  int i = 0;

  /* Perhaps should complain about this.
     Although the infinite loop might be doing useful work,
     "while (i < n)" is a misleading way to spell "infinite loop".  */
  while (i < n)
    maybe_useful_work ();
}

void test_non_volatile_local_3b (int n)
{
  int i = 0;

  while (i < n)
    {
      maybe_useful_work ();
      i++;
    }
}

void test_volatile_local (void)
{
  volatile int flag = 0;
  while (!flag) /* { dg-bogus "infinite loop" } */
    {
    }
}

void test_non_volatile_global (void)
{
  /* Not sure if we should warn here.  */
  while (!global_var) /* { dg-warning "infinite loop" } */
    {
    }
}

void test_volatile_global (void)
{
  while (!volatile_global_var) /* { dg-bogus "infinite loop" } */
    {
    }
}

void test_field_1 (struct st *p)
{
  /* Not sure if we should warn here.  */
  while (!p->x) /* { dg-warning "infinite loop" } */
    {
    }
}

void test_field_2 (struct st *p)
{
  while (!p->x) /* { dg-bogus "infinite loop" } */
    maybe_useful_work ();
}


int missing_init_of_i (int *arr, unsigned n)
{
  int sum = 0;
  for (int i; i < n; i--) /* { dg-warning "use of uninitialized value 'i'" } */
    sum += arr[i];
  return sum;
}

void test_switch (char *pc)
{
  while (1)
    {
      char opcode = *pc; /* { dg-warning "infinite loop" } */
      switch (opcode) /* { dg-message "if it ever follows 'default:' branch, it will always do so\.\.\." } */
	{
	case 'A':
	  pc++;
	  break;
	case 'B':
	  return;
	}
    }
}
