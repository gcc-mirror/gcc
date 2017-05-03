/* Test gcov block mode.  Compiler generates following CFG:

  <bb 3>:
  if (false_var_4 != 0)
    goto <bb 4>;
  else
    goto <bb 5>;

  <bb 4>:
  ret_6 = 111;
  PROF_edge_counter_10 = __gcov0.UuT[0];
  PROF_edge_counter_11 = PROF_edge_counter_10 + 1;
  __gcov0.UuT[0] = PROF_edge_counter_11;

  <bb 5>:
  # ret_1 = PHI <ret_5(3), ret_6(4)>
  goto <bb 7>;

It's important not to include <bb 5> to any line as it's actually shared
by both branches of the condition in <bb 3>.

*/

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

unsigned int
UuT (void)
{
  unsigned int true_var = 1;
  unsigned int false_var = 0;
  unsigned int ret = 0;

  if (true_var) /* count(1) */
    {
      if (false_var) /* count(1) */
	ret = 111; /* count(#####) */
    }
  else
    ret = 999; /* count(#####) */
  return ret;
}

int
main (int argc, char **argv)
{
  UuT ();
  return 0;
}

/* { dg-final { run-gcov { -a gcov-17.c } } } */
