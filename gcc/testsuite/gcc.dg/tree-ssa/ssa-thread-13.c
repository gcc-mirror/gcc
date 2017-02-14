/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ethread-details" } */
/* { dg-final { scan-tree-dump "FSM" "ethread" } } */

typedef struct rtx_def *rtx;
typedef const struct rtx_def *const_rtx;
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
union rtunion_def
{
  rtx rt_rtx;
};
typedef union rtunion_def rtunion;
struct rtx_def
{
  int code;
  union u
  {
    rtunion fld[1];
  }
  u;
};

unsigned int rtx_cost (rtx, enum rtx_code, unsigned char);
rtx single_set_2 (const_rtx, rtx);

unsigned
seq_cost (const_rtx seq, unsigned char speed)
{
  unsigned cost = 0;
  rtx set;
  for (; seq; seq = (((seq)->u.fld[2]).rt_rtx))
    {
      set =
	(((((enum rtx_code) (seq)->code) == INSN)
	  || (((enum rtx_code) (seq)->code) == DEBUG_INSN)
	  || (((enum rtx_code) (seq)->code) == JUMP_INSN)
	  || (((enum rtx_code) (seq)->code) ==
	      CALL_INSN)) ? (((enum rtx_code) ((((seq)->u.fld[4]).rt_rtx))->
			      code) ==
			     SET ? (((seq)->u.fld[4]).
				    rt_rtx) : single_set_2 (seq,
							    (((seq)->u.
							      fld[4]).
							     rt_rtx))) : (rtx)
	 0);
      if (set)
	cost += rtx_cost (set, SET, speed);
    }
}
