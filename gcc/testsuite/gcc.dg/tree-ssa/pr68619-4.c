/* { dg-do compile } */
/* { dg-options "-O2 -fno-code-hoisting -fdump-tree-optimized -w" } */

typedef struct rtx_def *rtx;
enum rtx_code
{
  UNKNOWN, VALUE, DEBUG_EXPR, EXPR_LIST, INSN_LIST, SEQUENCE, ADDRESS,
  DEBUG_INSN, INSN, JUMP_INSN, CALL_INSN, BARRIER, CODE_LABEL, NOTE,
  COND_EXEC, PARALLEL, ASM_INPUT, ASM_OPERANDS, UNSPEC, UNSPEC_VOLATILE,
  ADDR_VEC, ADDR_DIFF_VEC, PREFETCH, SET, USE, CLOBBER, CALL, RETURN,
  EH_RETURN, TRAP_IF, CONST_INT, CONST_FIXED, CONST_DOUBLE, CONST_VECTOR,
  CONST_STRING, CONST, PC, REG, SCRATCH, SUBREG, STRICT_LOW_PART, CONCAT,
  CONCATN, MEM, LABEL_REF, SYMBOL_REF, CC0, IF_THEN_ELSE, COMPARE, PLUS,
  MINUS, NEG, MULT, SS_MULT, US_MULT, DIV, SS_DIV, US_DIV, MOD, UDIV, UMOD,
  AND, IOR, XOR, NOT, ASHIFT, ROTATE, ASHIFTRT, LSHIFTRT, ROTATERT, SMIN,
  SMAX, UMIN, UMAX, PRE_DEC, PRE_INC, POST_DEC, POST_INC, PRE_MODIFY,
  POST_MODIFY, NE, EQ, GE, GT, LE, LT, GEU, GTU, LEU, LTU, UNORDERED,
  ORDERED, UNEQ, UNGE, UNGT, UNLE, UNLT, LTGT, SIGN_EXTEND, ZERO_EXTEND,
  TRUNCATE, FLOAT_EXTEND, FLOAT_TRUNCATE, FLOAT, FIX, UNSIGNED_FLOAT,
  UNSIGNED_FIX, FRACT_CONVERT, UNSIGNED_FRACT_CONVERT, SAT_FRACT,
  UNSIGNED_SAT_FRACT, ABS, SQRT, BSWAP, FFS, CLZ, CTZ, POPCOUNT, PARITY,
  SIGN_EXTRACT, ZERO_EXTRACT, HIGH, LO_SUM, VEC_MERGE, VEC_SELECT,
  VEC_CONCAT, VEC_DUPLICATE, SS_PLUS, US_PLUS, SS_MINUS, SS_NEG, US_NEG,
  SS_ABS, SS_ASHIFT, US_ASHIFT, US_MINUS, SS_TRUNCATE, US_TRUNCATE, FMA,
  VAR_LOCATION, DEBUG_IMPLICIT_PTR, ENTRY_VALUE, LAST_AND_UNUSED_RTX_CODE
};
enum rtx_class
{
  RTX_COMPARE, RTX_COMM_COMPARE, RTX_BIN_ARITH, RTX_COMM_ARITH, RTX_UNARY,
  RTX_EXTRA, RTX_MATCH, RTX_INSN, RTX_OBJ, RTX_CONST_OBJ, RTX_TERNARY,
  RTX_BITFIELD_OPS, RTX_AUTOINC
};
extern const unsigned char rtx_length[((int) LAST_AND_UNUSED_RTX_CODE)];
extern const enum rtx_class rtx_class[((int) LAST_AND_UNUSED_RTX_CODE)];
union rtunion_def
{
  rtx rt_rtx;
};
typedef union rtunion_def rtunion;
struct rtx_def
{
  enum rtx_code code:8;
  union u
  {
    rtunion fld[1];
  }
  u;
};
struct cse_reg_info
{
  unsigned int timestamp;
  int reg_qty;
  int reg_tick;
  int reg_in_table;
  unsigned int subreg_ticked;
};
static struct cse_reg_info *cse_reg_info_table;
static unsigned int cse_reg_info_timestamp;
int rhs_regno (rtx);
void foop (void);
void arf (void);

static __inline__ struct cse_reg_info *
get_cse_reg_info (unsigned int regno)
{
  struct cse_reg_info *p = &cse_reg_info_table[regno];
  if (p->timestamp != cse_reg_info_timestamp)
    cse_reg_info_table[regno].timestamp = cse_reg_info_timestamp;
}

int
mention_regs (rtx x)
{
  enum rtx_code code;
  int i, j;
  const char *fmt;
  int changed = 0;
  code = ((x)->code);
  if (code == SUBREG
      && ((((((x)->u.fld[0]).rt_rtx))->code) == REG))
    {
      (get_cse_reg_info (i)->reg_in_table) = (get_cse_reg_info (i)->reg_tick);
      (get_cse_reg_info (i)->subreg_ticked) =
	(rhs_regno ((((x)->u.fld[0]).rt_rtx)));
    }
  if ((((rtx_class[(int) (((x)->code))]) & (~1)) == (RTX_COMPARE & (~1))))
    {
      if (((((((x)->u.fld[0]).rt_rtx))->code) == REG))
	foop ();
    }
  for (i = (rtx_length[(int) (code)]) - 1; i >= 0; i--)
	arf ();
}

/* Make sure the constant 39 gets propagated into the PHI at the join point.  */
/* { dg-final { scan-tree-dump "PHI <.*, 39" "optimized"} } */

