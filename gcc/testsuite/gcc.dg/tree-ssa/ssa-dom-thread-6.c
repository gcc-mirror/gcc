/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-thread1-details -fdump-tree-thread2-details" } */

/* All the threads in the thread1 dump start on a X->BB12 edge, as can
   be seen in the dump:

     Registering FSM jump thread: (x, 12) incoming edge; ...
     etc
     etc

   Before the new evrp, we were threading paths that started at the
   following edges:

      Registering FSM jump thread: (10, 12) incoming edge
      Registering FSM jump thread:  (6, 12) incoming edge
      Registering FSM jump thread:  (9, 12) incoming edge

   This was because the PHI at BB12 had constant values coming in from
   BB10, BB6, and BB9:

   # state_10 = PHI <state_11(7), 0(10), state_11(5), 1(6), state_11(8), 2(9), state_11(11)>

   Now with the new evrp, we get:

   # state_10 = PHI <0(7), 0(10), state_11(5), 1(6), 0(8), 2(9), 1(11)>

   Thus, we have 3 more paths that are known to be constant and can be
   threaded.  Which means that by the second threading pass, we can
   only find one profitable path.

   For the record, all these extra constants are better paths coming
   out of switches.  For example:

     SWITCH_BB -> BBx -> BBy -> BBz -> PHI

   We now know the value of the switch index at PHI.  */
/* { dg-final { scan-tree-dump-times "FSM" 6 "thread1" } } */
/* { dg-final { scan-tree-dump-times "FSM" 1 "thread2" } } */

int sum0, sum1, sum2, sum3;
int foo (char *s, char **ret)
{
  int state=0;
  char c;

  for (; *s && state != 4; s++)
    {
      c = *s;
      if (c == '*')
	{
	  s++;
	  break;
	}
      switch (state)
	{
	case 0:
	  if (c == '+')
	    state = 1;
	  else if (c != '-')
	    sum0+=c;
	  break;
	case 1:
	  if (c == '+')
	    state = 2;
	  else if (c == '-')
	    state = 0;
	  else
	    sum1+=c;
	  break;
	default:
	  break;
	}

    }
  *ret = s;
  return state;
}
