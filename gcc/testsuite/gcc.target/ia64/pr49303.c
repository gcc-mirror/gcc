/* { dg-do compile } */
/* { dg-options "-w -O2 -fselective-scheduling2 -fsel-sched-pipelining" } */

typedef struct rtx_def *rtx;
typedef const struct rtx_def *const_rtx;
typedef struct basic_block_def *basic_block;
enum machine_mode {
  VOIDmode, BLKmode, CCmode, CCImode, BImode, QImode, HImode, SImode, DImode, TImode, OImode, QQmode, HQmode, SQmode, DQmode, TQmode, UQQmode, UHQmode, USQmode, UDQmode, UTQmode, HAmode, SAmode, DAmode, TAmode, UHAmode, USAmode, UDAmode, UTAmode, SFmode, DFmode, XFmode, RFmode, TFmode, SDmode, DDmode, TDmode, CQImode, CHImode, CSImode, CDImode, CTImode, COImode, SCmode, DCmode, XCmode, RCmode, TCmode, V4QImode, V2HImode, V8QImode, V4HImode, V2SImode, V16QImode, V8HImode, V4SImode, V2SFmode, V4SFmode, MAX_MACHINE_MODE, MIN_MODE_RANDOM = VOIDmode, MAX_MODE_RANDOM = BLKmode, MIN_MODE_CC = CCmode, MAX_MODE_CC = CCImode, MIN_MODE_INT = QImode, MAX_MODE_INT = OImode, MIN_MODE_PARTIAL_INT = VOIDmode, MAX_MODE_PARTIAL_INT = VOIDmode, MIN_MODE_FRACT = QQmode, MAX_MODE_FRACT = TQmode, MIN_MODE_UFRACT = UQQmode, MAX_MODE_UFRACT = UTQmode, MIN_MODE_ACCUM = HAmode, MAX_MODE_ACCUM = TAmode, MIN_MODE_UACCUM = UHAmode, MAX_MODE_UACCUM = UTAmode, MIN_MODE_FLOAT = SFmode, MAX_MODE_FLOAT = TFmode, MIN_MODE_DECIMAL_FLOAT = SDmode, MAX_MODE_DECIMAL_FLOAT = TDmode, MIN_MODE_COMPLEX_INT = CQImode, MAX_MODE_COMPLEX_INT = COImode, MIN_MODE_COMPLEX_FLOAT = SCmode, MAX_MODE_COMPLEX_FLOAT = TCmode, MIN_MODE_VECTOR_INT = V4QImode, MAX_MODE_VECTOR_INT = V4SImode, MIN_MODE_VECTOR_FRACT = VOIDmode, MAX_MODE_VECTOR_FRACT = VOIDmode, MIN_MODE_VECTOR_UFRACT = VOIDmode, MAX_MODE_VECTOR_UFRACT = VOIDmode, MIN_MODE_VECTOR_ACCUM = VOIDmode, MAX_MODE_VECTOR_ACCUM = VOIDmode, MIN_MODE_VECTOR_UACCUM = VOIDmode, MAX_MODE_VECTOR_UACCUM = VOIDmode, MIN_MODE_VECTOR_FLOAT = V2SFmode, MAX_MODE_VECTOR_FLOAT = V4SFmode, NUM_MACHINE_MODES = MAX_MACHINE_MODE };
struct real_value {
};
extern void vec_assert_fail (const char *, const char * ,const char *file_,unsigned line_,const char *function_) __attribute__ ((__noreturn__));
typedef struct vec_prefix {
  unsigned num;
};
enum rtx_code {
  UNKNOWN , VALUE , DEBUG_EXPR , EXPR_LIST , INSN_LIST , SEQUENCE , ADDRESS , DEBUG_INSN , INSN , JUMP_INSN , CALL_INSN , BARRIER , CODE_LABEL , NOTE , COND_EXEC , PARALLEL , ASM_INPUT , ASM_OPERANDS , UNSPEC , UNSPEC_VOLATILE , ADDR_VEC , ADDR_DIFF_VEC , PREFETCH , SET , USE , CLOBBER , CALL , RETURN , EH_RETURN , TRAP_IF , CONST_INT , CONST_FIXED , CONST_DOUBLE , CONST_VECTOR , CONST_STRING , CONST , PC , REG , SCRATCH , SUBREG , STRICT_LOW_PART , CONCAT , CONCATN , MEM , LABEL_REF , SYMBOL_REF , CC0 , IF_THEN_ELSE , COMPARE , PLUS , MINUS , NEG , MULT , SS_MULT , US_MULT , DIV , SS_DIV , US_DIV , MOD , UDIV , UMOD , AND , IOR , XOR , NOT , ASHIFT , ROTATE , ASHIFTRT , LSHIFTRT , ROTATERT , SMIN , SMAX , UMIN , UMAX , PRE_DEC , PRE_INC , POST_DEC , POST_INC , PRE_MODIFY , POST_MODIFY , NE , EQ , GE , GT , LE , LT , GEU , GTU , LEU , LTU , UNORDERED , ORDERED , UNEQ , UNGE , UNGT , UNLE , UNLT , LTGT , SIGN_EXTEND , ZERO_EXTEND , TRUNCATE , FLOAT_EXTEND , FLOAT_TRUNCATE , FLOAT , FIX , UNSIGNED_FLOAT , UNSIGNED_FIX , FRACT_CONVERT , UNSIGNED_FRACT_CONVERT , SAT_FRACT , UNSIGNED_SAT_FRACT , ABS , SQRT , BSWAP , FFS , CLZ , CTZ , POPCOUNT , PARITY , SIGN_EXTRACT , ZERO_EXTRACT , HIGH , LO_SUM , VEC_MERGE , VEC_SELECT , VEC_CONCAT , VEC_DUPLICATE , SS_PLUS , US_PLUS , SS_MINUS , SS_NEG , US_NEG , SS_ABS , SS_ASHIFT , US_ASHIFT , US_MINUS , SS_TRUNCATE , US_TRUNCATE , FMA , VAR_LOCATION , DEBUG_IMPLICIT_PTR , ENTRY_VALUE , LAST_AND_UNUSED_RTX_CODE};
enum rtx_class {
  RTX_COMPARE, RTX_COMM_COMPARE, RTX_BIN_ARITH, RTX_COMM_ARITH, RTX_UNARY, RTX_EXTRA, RTX_MATCH, RTX_INSN, RTX_OBJ, RTX_CONST_OBJ, RTX_TERNARY, RTX_BITFIELD_OPS, RTX_AUTOINC };
extern const enum rtx_class rtx_class[((int) LAST_AND_UNUSED_RTX_CODE)];
union rtunion_def {
  int rt_int;
  unsigned int rt_uint;
  rtx rt_rtx;
};
typedef union rtunion_def rtunion;
struct rtx_def {
  __extension__ enum rtx_code code: 16;
  __extension__ enum machine_mode mode : 8;
  unsigned int unchanging : 1;
  union u {
    rtunion fld[1];
  }
  u;
};
static __inline__ unsigned int rhs_regno (const_rtx x) {
  return (((x)->u.fld[0]).rt_uint);
}
struct regstat_n_sets_and_refs_t {
  int sets;
};
extern struct regstat_n_sets_and_refs_t *regstat_n_sets_and_refs;
static __inline__ int REG_N_SETS (int regno) {
  return regstat_n_sets_and_refs[regno].sets;
}
struct target_regs {
  unsigned char x_hard_regno_nregs[334][MAX_MACHINE_MODE];
};
extern struct target_regs default_target_regs;
static __inline__ unsigned int end_hard_regno (enum machine_mode mode, unsigned int regno) {
  return regno + ((&default_target_regs)->x_hard_regno_nregs)[regno][(int) mode];
}
struct function {
  struct eh_status *eh;
  struct control_flow_graph *cfg;
};
extern struct function *cfun;
typedef struct VEC_edge_gc {
}
VEC_edge_gc;
struct basic_block_def {
  VEC_edge_gc *preds;
  struct basic_block_def *next_bb;
  int index;
}
VEC_basic_block_gc;
struct control_flow_graph {
  basic_block x_entry_block_ptr;
}
bitmap_obstack;
typedef struct bitmap_element_def {
}
bitmap_element;
typedef struct bitmap_head_def {
  bitmap_element *first;
  bitmap_element *current;
}
bitmap_head;
struct dataflow {
  struct df_problem *problem;
  void *block_info;
  unsigned int block_info_size;
};
struct df_insn_info {
  int luid;
};
struct df_d {
  struct dataflow *problems_by_index[(7 + 1)];
  struct df_insn_info **insns;
};
struct df_lr_bb_info {
  bitmap_head def;
  bitmap_head in;
};
extern struct df_d *df;
static __inline__ struct df_lr_bb_info * df_lr_get_bb_info (unsigned int index) {
  if (index < (df->problems_by_index[1])->block_info_size) return &((struct df_lr_bb_info *) (df->problems_by_index[1])->block_info)[index];
  else return ((void *)0);
}
typedef struct reg_stat_struct {
  int last_set_label;
  unsigned long last_set_nonzero_bits;
  char last_set_invalid;
}
reg_stat_type;
typedef struct VEC_reg_stat_type_base {
  struct vec_prefix prefix;
  reg_stat_type vec[1];
}
VEC_reg_stat_type_base;
static __inline__ reg_stat_type *VEC_reg_stat_type_base_index (VEC_reg_stat_type_base *vec_, unsigned ix_ ,const char *file_,unsigned line_,const char *function_) {
  (void)((vec_ && ix_ < vec_->prefix.num) ? 0 : (vec_assert_fail ("index","VEC(reg_stat_type,base)" ,file_,line_,function_), 0));
  return &vec_->vec[ix_];
}
typedef struct VEC_reg_stat_type_heap {
  VEC_reg_stat_type_base base;
}
VEC_reg_stat_type_heap;
static VEC_reg_stat_type_heap *reg_stat;
static int mem_last_set;
static int label_tick;
int get_last_value_validate (rtx *loc, rtx insn, int tick, int replace)
{
  rtx x = *loc;
  int i, j;
  if ((((enum rtx_code) (x)->code) == REG))
    {
      unsigned int regno = (rhs_regno(x));
      unsigned int endregno = (((((rhs_regno(x))) < 334)) ? end_hard_regno (((enum machine_mode) (x)->mode), (rhs_regno(x))) : (rhs_regno(x)) + 1);
      for (j = regno;
	   j < endregno;
	   j++)
	{
	  reg_stat_type *rsp = (VEC_reg_stat_type_base_index(((reg_stat) ? &(reg_stat)->base : 0),j ,"/gcc/combine.c",12640,__FUNCTION__));
	  if (
	      rsp->last_set_invalid
	      ||
	      (
	       (
		regno >= 334
		&& REG_N_SETS (regno) == 1
		&& (!bitmap_bit_p ((&(df_lr_get_bb_info((((cfun + 0)->cfg->x_entry_block_ptr)->next_bb)->index))->in), regno) )
	       )
	       && rsp->last_set_label > tick
	      )
	     )
	    {
	      return replace;
	    }
	}
    }
  else if ((((enum rtx_code) (x)->code) == MEM)
	   &&
	   (
	    (
	     {
	     __typeof ((x)) const _rtx = ((x));
	     _rtx;
	     }
	    )->unchanging
	   )
	   &&
	   (
	    tick != label_tick
	    || ((((df->insns[((((insn)->u.fld[0]).rt_int))]))->luid)) <= mem_last_set
	   )
	  )
    {
	{
	  if (
	      i == 1
	     )
	    {
	      rtx x0 = (((x)->u.fld[0]).rt_rtx);
	      rtx x1 = (((x)->u.fld[1]).rt_rtx);
	      if ((((rtx_class[(int) (((enum rtx_code) (x1)->code))]) & (~1)) == (RTX_COMM_ARITH & (~1)))
		  &&
		  (
		   x0 == (((x1)->u.fld[0]).rt_rtx)
		  )
		 )
		return get_last_value_validate (&(((x1)->u.fld[x0 == (((x1)->u.fld[0]).rt_rtx) ? 1 : 0]).rt_rtx) , insn, tick, replace);
	    }
	}
    }
}
