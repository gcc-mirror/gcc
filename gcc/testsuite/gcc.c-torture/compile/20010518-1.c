/* This was cut down from reload1.c in May 2001, was observed to cause
   a bootstrap failure for powerpc-apple-darwin1.3.

   Copyright (C) 2001  Free Software Foundation.  */

enum insn_code
{
  CODE_FOR_extendqidi2 = 3,
  CODE_FOR_nothing = 870
};

struct rtx_def;

enum machine_mode
{
  VOIDmode,
  MAX_MACHINE_MODE
};

typedef unsigned long long HARD_REG_ELT_TYPE;
typedef HARD_REG_ELT_TYPE HARD_REG_SET[((77 + (8 * 8) - 1) / (8 * 8))];

enum rtx_code
{
  UNKNOWN,
  NIL,
  REG,
  LAST_AND_UNUSED_RTX_CODE
};

typedef struct
{
  unsigned min_align:8;
  unsigned base_after_vec:1;
  unsigned min_after_vec:1;
  unsigned max_after_vec:1;
  unsigned min_after_base:1;
  unsigned max_after_base:1;
  unsigned offset_unsigned:1;
  unsigned:2;
  unsigned scale:8;
}
addr_diff_vec_flags;
typedef union rtunion_def
{
  long long rtwint;
  int rtint;
  unsigned int rtuint;
  const char *rtstr;
  struct rtx_def *rtx;
  struct rtvec_def *rtvec;
  enum machine_mode rttype;
  addr_diff_vec_flags rt_addr_diff_vec_flags;
  struct cselib_val_struct *rt_cselib;
  struct bitmap_head_def *rtbit;
  union tree_node *rttree;
  struct basic_block_def *bb;
}
rtunion;
typedef struct rtx_def
{
  enum rtx_code code:16;
  enum machine_mode mode:8;
  unsigned int jump:1;
  unsigned int call:1;
  unsigned int unchanging:1;
  unsigned int volatil:1;
  unsigned int in_struct:1;
  unsigned int used:1;
  unsigned integrated:1;
  unsigned frame_related:1;
  rtunion fld[1];
}
 *rtx;

enum reload_type
{
  RELOAD_FOR_INPUT, RELOAD_FOR_OUTPUT, RELOAD_FOR_INSN,
  RELOAD_FOR_INPUT_ADDRESS, RELOAD_FOR_INPADDR_ADDRESS,
  RELOAD_FOR_OUTPUT_ADDRESS, RELOAD_FOR_OUTADDR_ADDRESS,
  RELOAD_FOR_OPERAND_ADDRESS, RELOAD_FOR_OPADDR_ADDR,
  RELOAD_OTHER, RELOAD_FOR_OTHER_ADDRESS
};

struct reload
{
  rtx in;
  rtx out;
  //  enum reg_class class;
  enum machine_mode inmode;
  enum machine_mode outmode;
  enum machine_mode mode;
  unsigned int nregs;
  int inc;
  rtx in_reg;
  rtx out_reg;
  int regno;
  rtx reg_rtx;
  int opnum;
  int secondary_in_reload;
  int secondary_out_reload;
  enum insn_code secondary_in_icode;
  enum insn_code secondary_out_icode;
  enum reload_type when_needed;
  unsigned int optional:1;
  unsigned int nocombine:1;
  unsigned int secondary_p:1;
  unsigned int nongroup:1;
};

struct insn_chain
{
  rtx insn;
};

extern int n_reloads;
static short reload_order[(2 * 10 * (2 + 1))];
int reload_spill_index[(2 * 10 * (2 + 1))];
extern struct reload rld[(2 * 10 * (2 + 1))];
static rtx *reg_last_reload_reg;
static HARD_REG_SET reg_reloaded_valid;
static HARD_REG_SET reg_reloaded_dead;
static HARD_REG_SET reg_reloaded_died;
static HARD_REG_SET reg_is_output_reload;
extern const unsigned int mode_size[];
extern int target_flags;

static void
emit_reload_insns (chain)
     struct insn_chain *chain;
{
  rtx insn = chain->insn;
  register int j;
  rtx following_insn = (((insn)->fld[2]).rtx);
  rtx before_insn = (((insn)->fld[1]).rtx);

  for (j = 0; j < n_reloads; j++)
    {
      register int r = reload_order[j];
      register int i = reload_spill_index[r];

	{
	  rtx out = (((enum rtx_code) (rld[r].out)->code) == REG ? rld[r].out : rld[r].out_reg);
	  register int nregno = (((out)->fld[0]).rtuint);

	  if (nregno >= 77)
	    {
	      rtx src_reg, store_insn = (rtx) 0;

	      reg_last_reload_reg[nregno] = 0;
	      if (src_reg && ((enum rtx_code) (src_reg)->code) == REG && (((src_reg)->fld[0]).rtuint) < 77)
		{
		  int src_regno = (((src_reg)->fld[0]).rtuint);
		  int nr =
		    (((src_regno) >= 32
		      && (src_regno) <=
		      63) ? (((mode_size[(int) (rld[r].mode)]) + 8 -
			      1) / 8) : (((mode_size[(int) (rld[r].mode)]) +
					  (!(target_flags & 0x00000020) ? 4 :
					   8) - 1) / (!(target_flags & 0x00000020) ? 4 : 8)));
		  rtx note = 0;

		  while (nr-- > 0)
		    {
		      ((reg_reloaded_dead)
		       [(src_regno + nr) / ((unsigned) (8 * 8))] &=
		       ~(((HARD_REG_ELT_TYPE) (1)) << ((src_regno + nr) % ((unsigned) (8 * 8)))));
		      ((reg_reloaded_valid)
		       [(src_regno + nr) / ((unsigned) (8 * 8))] |=
		       ((HARD_REG_ELT_TYPE) (1)) << ((src_regno + nr) % ((unsigned) (8 * 8))));
		      ((reg_is_output_reload)
		       [(src_regno + nr) / ((unsigned) (8 * 8))] |=
		       ((HARD_REG_ELT_TYPE) (1)) << ((src_regno + nr) % ((unsigned) (8 * 8))));
		      if (note)
			((reg_reloaded_died)
			 [(src_regno) / ((unsigned) (8 * 8))] |=
			 ((HARD_REG_ELT_TYPE) (1)) << ((src_regno) % ((unsigned) (8 * 8))));
		      else
			((reg_reloaded_died)
			 [(src_regno) / ((unsigned) (8 * 8))] &=
			 ~(((HARD_REG_ELT_TYPE) (1)) << ((src_regno) % ((unsigned) (8 * 8)))));
		    }
		  reg_last_reload_reg[nregno] = src_reg;
		}
	    }
	  else
	    {
	      int num_regs =
		(((nregno) >= 32
		  && (nregno) <=
		  63)
		 ? (((mode_size
		      [(int) (((enum machine_mode) (rld[r].out)->mode))]) +
		     8 -
		     1) /
		    8)
		 : (((mode_size
		      [(int) (((enum machine_mode) (rld[r].out)->mode))]) +
		     (!(target_flags & 0x00000020) ? 4 : 8) - 1) / (!(target_flags & 0x00000020) ? 4 : 8)));
	      while (num_regs-- > 0)
		reg_last_reload_reg[nregno + num_regs] = 0;
	    }
	}
    }
}
