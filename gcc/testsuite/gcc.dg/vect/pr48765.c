/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { *-*-* } { "-mcpu=*" } { "-mcpu=power6" } } */
/* { dg-additional-options "-O3 -mcpu=power6" } */

enum reg_class
{
  NO_REGS, AP_REG, XRF_REGS, GENERAL_REGS, AGRF_REGS, XGRF_REGS, ALL_REGS,
    LIM_REG_CLASSES
};
enum machine_mode
{
  VOIDmode, QImode, HImode, PSImode, SImode, PDImode, DImode, TImode, OImode,
    QFmode, HFmode, TQFmode, SFmode, DFmode, XFmode, TFmode, SCmode, DCmode,
    XCmode, TCmode, CQImode, CHImode, CSImode, CDImode, CTImode, COImode,
    BLKmode, CCmode, CCEVENmode, MAX_MACHINE_MODE
};
typedef struct rtx_def
{
  int mode:8;
}
 *rtx;
extern rtx *regno_reg_rtx;
typedef unsigned int HARD_REG_ELT_TYPE;
typedef HARD_REG_ELT_TYPE HARD_REG_SET[((64 + 32 - 1) / 32)];
extern int reg_alloc_order[64];
extern int max_regno;
extern int *reg_n_calls_crossed;
extern short *reg_renumber;
static int *reg_where_dead;
static int *reg_where_born;
static int *reg_order;
static char *regs_change_size;
static HARD_REG_SET *after_insn_hard_regs;
static int stupid_find_reg (int, enum reg_class, enum machine_mode, int, int,
			    int);
enum reg_class reg_preferred_class (int);
void
stupid_life_analysis (f, nregs, file)
     int nregs, file;
     rtx f;
{
  register int i;
  for (i = (((64)) + 3) + 1; i < max_regno; i++)
    {
      register int r = reg_order[i];
      if ((int) LIM_REG_CLASSES > 1)
	reg_renumber[r] =
	  stupid_find_reg (reg_n_calls_crossed[r], reg_preferred_class (r),
			   ((regno_reg_rtx[r])->mode), reg_where_born[r],
			   reg_where_dead[r], regs_change_size[r]);
    }
}

static int
stupid_find_reg (call_preserved, class, mode, born_insn, dead_insn,
		 changes_size)
     int call_preserved, born_insn, dead_insn, changes_size;
     enum reg_class class;
     enum machine_mode mode;
{
  register int i, ins;
  HARD_REG_SET used, this_reg;
  for (ins = born_insn; ins < dead_insn; ins++)
    do
      {
	register HARD_REG_ELT_TYPE *scan_tp_ = (used), *scan_fp_ =
	  (after_insn_hard_regs[ins]);
	for (i = 0; i < ((64 + 32 - 1) / 32); i++)
	  *scan_tp_++ |= *scan_fp_++;
      }
    while (0);
  for (i = 0; i < 64; i++)
    {
      int regno = reg_alloc_order[i];
      if (((used)[(regno) / ((unsigned) 32)] &
	   (((HARD_REG_ELT_TYPE) (1)) << ((regno) % ((unsigned) 32)))))
	{
	  register int j;
	  if (j == regno)
	    return regno;
	}
    }
}

