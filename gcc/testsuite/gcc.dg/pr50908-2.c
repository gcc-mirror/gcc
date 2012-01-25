/* { dg-do compile } */
/* { dg-options "-O2 -ftree-tail-merge -fno-short-enums" } */

typedef struct rtx_def *rtx;
enum debug_info_levels
{
  ARM_FLOAT_ABI_SOFT, ARM_FLOAT_ABI_SOFTFP, ARM_FLOAT_ABI_HARD
};
struct gcc_options
{
  int x_target_flags;
};
extern struct gcc_options global_options;
extern int arm_arch_thumb2;
enum rtx_code
{
  UNSPEC, UNSPEC_VOLATILE, ADDR_VEC, SET, CLOBBER, CALL, RETURN,
    SIMPLE_RETURN, EH_RETURN, TRAP_IF, CONST_INT, CONST_FIXED, CONST_DOUBLE,
    CONST_VECTOR, CONST_STRING, CONST, PC, REG, SCRATCH, SUBREG,
    STRICT_LOW_PART, CONCAT, CONCATN, MEM, LABEL_REF, SYMBOL_REF, CC0,
    IF_THEN_ELSE, COMPARE, PLUS, MINUS, NEG, MULT, SS_MULT, US_MULT, DIV,
    SS_DIV, US_DIV, MOD, UDIV, UMOD, AND, IOR, XOR, NOT, ASHIFT, ROTATE,
    ASHIFTRT, LSHIFTRT, ROTATERT, PRE_DEC, PRE_INC, POST_DEC, POST_INC,
    PRE_MODIFY, POST_MODIFY, NE, EQ, GE, GT, LE, LT, GEU, GTU, LEU, LTU,
    UNORDERED, ORDERED, UNEQ, UNGE, UNGT, UNLE, UNLT, LTGT, SIGN_EXTEND,
    ZERO_EXTEND, TRUNCATE, FLOAT_EXTEND, FLOAT_TRUNCATE, FLOAT, FIX,
    UNSIGNED_FLOAT, UNSIGNED_FIX, SIGN_EXTRACT, ZERO_EXTRACT, HIGH, LO_SUM,
    VEC_MERGE, VEC_SELECT, VEC_CONCAT, VEC_DUPLICATE, SS_PLUS, US_PLUS,
    SS_MINUS, SS_NEG, US_NEG, SS_ABS, SS_ASHIFT, US_ASHIFT, US_MINUS,
    SS_TRUNCATE, US_TRUNCATE, FMA, VAR_LOCATION, DEBUG_IMPLICIT_PTR,
    ENTRY_VALUE, DEBUG_PARAMETER_REF, LAST_AND_UNUSED_RTX_CODE
};
union rtunion_def
{
};
struct rtx_def
{
  enum rtx_code code:16;
}
builtin_info_type;
enum constraint_num
{
  CONSTRAINT__UNKNOWN =
    0, CONSTRAINT_f, CONSTRAINT_t, CONSTRAINT_v, CONSTRAINT_w, CONSTRAINT_x,
    CONSTRAINT_y, CONSTRAINT_z, CONSTRAINT_l, CONSTRAINT_h, CONSTRAINT_j,
    CONSTRAINT_Pj, CONSTRAINT_PJ, CONSTRAINT_k, CONSTRAINT_b, CONSTRAINT_c,
    CONSTRAINT_I, CONSTRAINT_J, CONSTRAINT_K, CONSTRAINT_L, CONSTRAINT_M,
    CONSTRAINT_N, CONSTRAINT_O, CONSTRAINT_Pa, CONSTRAINT_Pb, CONSTRAINT_Pc,
    CONSTRAINT_Pd, CONSTRAINT_Ps, CONSTRAINT_Pt, CONSTRAINT_Pu, CONSTRAINT_Pv,
    CONSTRAINT_Pw, CONSTRAINT_Px, CONSTRAINT_Py, CONSTRAINT_G, CONSTRAINT_H,
    CONSTRAINT_Dz, CONSTRAINT_Da, CONSTRAINT_Db, CONSTRAINT_Dc, CONSTRAINT_Di,
    CONSTRAINT_Dn, CONSTRAINT_Dl, CONSTRAINT_DL, CONSTRAINT_Dv, CONSTRAINT_Dy,
    CONSTRAINT_Ut, CONSTRAINT_Uv, CONSTRAINT_Uy, CONSTRAINT_Un, CONSTRAINT_Um,
    CONSTRAINT_Us, CONSTRAINT_Uq, CONSTRAINT_Q, CONSTRAINT_Uu, CONSTRAINT_Uw,
    CONSTRAINT__LIMIT
};
typedef struct VEC_char_base
{
}
VEC_int_heap;
static inline int
satisfies_constraint_j (rtx op)
{
  long long ival = 0;
  return ((((!((global_options.x_target_flags & (1 << 14)) != 0))
	    || arm_arch_thumb2) && arm_arch_thumb2))
    && ((((enum rtx_code) (op)->code) == HIGH)
	|| ((((enum rtx_code) (op)->code) == CONST_INT)
	    && (((ival & 0xffff0000) == 0))));
}

int
constraint_satisfied_p (rtx op, enum constraint_num c)
{
  switch (c)
    {
    case CONSTRAINT_j:
      return satisfies_constraint_j (op);
    }
}
