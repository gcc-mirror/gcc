/* { dg-do compile } */
/* { dg-options "-O1 -g" } */

enum reg_class
{
  BASE_REGS,
  GENERAL_REGS,
  LIM_REG_CLASSES
};

static __inline__ unsigned char
hard_reg_set_subset_p (const unsigned long x[4], const unsigned long y[4])
{
  return ((x[0] & ~y[0]) == 0
          && (x[1] & ~y[1]) == 0
          && (x[2] & ~y[2]) == 0
	  && (x[3] & ~y[3]) == 0);
}

static __inline__ unsigned char
hard_reg_set_equal_p (const unsigned long x[4], const unsigned long y[4])
{
  return x[0] == y[0]
         && x[1] == y[1]
         && x[2] == y[2]
         && x[3] == y[3];
}

extern unsigned long reg_class_contents[(int) LIM_REG_CLASSES][4];
extern int ira_important_classes_num;
extern enum reg_class ira_important_classes[(int) LIM_REG_CLASSES];
extern enum reg_class ira_reg_class_intersect[(int) LIM_REG_CLASSES][(int)
								     LIM_REG_CLASSES];
extern unsigned char ira_reg_classes_intersect_p[(int) LIM_REG_CLASSES][(int)
									LIM_REG_CLASSES];
extern enum reg_class ira_reg_class_super_classes[(int) LIM_REG_CLASSES][(int)
									 LIM_REG_CLASSES];
static unsigned long temp_hard_regset[4];

static void
setup_reg_class_relations (void)
{
  int i, cl1, cl2, cl3;
  unsigned long temp_set2[4];
  for (cl1 = 0; cl1 < (int) LIM_REG_CLASSES; cl1++)
    {
      ira_reg_class_super_classes[cl1][0] = LIM_REG_CLASSES;
      for (cl2 = 0; cl2 < (int) LIM_REG_CLASSES; cl2++)
	{
	  ira_reg_classes_intersect_p[cl1][cl2] = 0;
	  {
	    unsigned long *scan_tp_ = (temp_set2), *scan_fp_ =
	      (reg_class_contents[cl2]);
	    scan_tp_[1] = scan_fp_[1];
	    scan_tp_[2] = scan_fp_[2];
	    scan_tp_[3] = scan_fp_[3];
	  }
	  for (i = 0; i < ira_important_classes_num; i++)
	    {
	      cl3 = ira_important_classes[i];
	      {
		unsigned long *scan_tp_ = (temp_hard_regset), *scan_fp_ =
		  (reg_class_contents[cl3]);
		scan_tp_[0] = scan_fp_[0];
		scan_tp_[1] = scan_fp_[1];
		scan_tp_[3] = scan_fp_[3];
	      }
	      if (!hard_reg_set_subset_p (temp_hard_regset, temp_set2)
		  || (hard_reg_set_equal_p (temp_hard_regset, temp_set2)
		      && hard_reg_set_subset_p (reg_class_contents[cl3],
						reg_class_contents[(int)
								   ira_reg_class_intersect
								   [cl1]
								   [cl2]])))
		ira_reg_class_intersect[cl1][cl2] = (enum reg_class) cl3;
	    }
	}
    }
}

static void
find_reg_class_closure (void)
{
  setup_reg_class_relations ();
}

void
ira_init (void)
{
  find_reg_class_closure ();
}
