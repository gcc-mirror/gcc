/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=2" } */

#include <stdbool.h>

typedef unsigned long HARD_REG_ELT_TYPE;
typedef HARD_REG_ELT_TYPE HARD_REG_SET[1];
struct target_ira
{
  HARD_REG_SET x_ira_prohibited_class_mode_regs[1][1];
};
extern struct target_ira *this_target_ira;
static inline bool
ira_object_conflict_iter_cond ()
{
}

static inline bool
check_hard_reg_p (int num_objects, int hard_regno,
		  HARD_REG_SET * conflict_regs, HARD_REG_SET profitable_regs)
{
  int j, nwords, nregs;
  if ((! !
       (((this_target_ira->
	  x_ira_prohibited_class_mode_regs)[0][0])[(hard_regno) /
						   ((unsigned) (8 * 8))] &
	(((HARD_REG_ELT_TYPE) (1)) <<
	 ((hard_regno) % ((unsigned) (8 * 8)))))))
    return false;
  nwords = num_objects;
  for (j = 0; j < nregs; j++)
    {
      int k;
      int set_to_test_start = 0, set_to_test_end = nwords;
      if (nregs == nwords)
	{
	  if (0)
	    set_to_test_start = nwords - j - 1;
	  else
	    set_to_test_start = j;
	}
      for (k = set_to_test_start; k < set_to_test_end; k++)
	if ((! !
	     ((conflict_regs[k])[(hard_regno + j) / ((unsigned) (8 * 8))] &
	      (((HARD_REG_ELT_TYPE) (1)) <<
	       ((hard_regno + j) % ((unsigned) (8 * 8)))))))
	  break;
      if (k != set_to_test_end)
	break;
    }
  return j == nregs;
}

void
improve_allocation (void)
{
  int j, k, n, hregno, conflict_hregno, base_cost, class_size, word, nwords;
  int check, spill_cost, min_cost, nregs, conflict_nregs, r, best;
  int costs[81];
  HARD_REG_SET conflicting_regs[2], profitable_hard_regs;
  int a;
  for (;;)
    {
      nwords = a;
      for (word = 0; word < nwords; word++)
	{
	  for (; ira_object_conflict_iter_cond ();)
	    {
	      for (r = conflict_hregno;
		   r < conflict_hregno + conflict_nregs; r++)
		if (check_hard_reg_p
		    (a, r, conflicting_regs, profitable_hard_regs))
		  costs[r] += spill_cost;
	    }
	  if (check_hard_reg_p
	      (a, hregno, conflicting_regs, profitable_hard_regs)
	      && min_cost > costs[hregno])
	    {
	      best = hregno;
	    }
	  for (; ira_object_conflict_iter_cond ();)
	    {
	      if (best + nregs <= conflict_hregno
		  || conflict_hregno + conflict_nregs <= best)
		continue;
	    }
	}
    }
}
