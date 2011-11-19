/* PR rtl-optimization/51187 */
/* Reported by Jurij Smakov <jurij@wooyd.org> */

/* { dg-do compile } */
/* { dg-options "-g -O2" } */

extern int printf (__const char *__restrict __format, ...);
extern void print_c_condition (const char *);

enum decision_type
{
  DT_num_insns,
  DT_mode, DT_code, DT_veclen,
  DT_elt_zero_int, DT_elt_one_int, DT_elt_zero_wide, DT_elt_zero_wide_safe,
  DT_const_int,
  DT_veclen_ge, DT_dup, DT_pred, DT_c_test,
  DT_accept_op, DT_accept_insn
};

struct decision_test
{
  struct decision_test *next;
  enum decision_type type;

  union
  {
    int num_insns;

    struct
    {
      const char *name;
    } pred;

    const char *c_test;
    int veclen;
    int dup;
    long intval;
    int opno;

    struct {
      int code_number;
      int lineno;
      int num_clobbers_to_add;
    } insn;
  } u;
};

enum routine_type {
  RECOG, SPLIT, PEEPHOLE2
};

void
write_cond (struct decision_test *p, int depth,
     enum routine_type subroutine_type)
{
  switch (p->type)
    {
    case DT_num_insns:
      printf ("peep2_current_count >= %d", p->u.num_insns);
      break;

    case DT_code:
      printf ("GET_CODE (x%d) == ", depth);
      break;

    case DT_veclen:
      printf ("XVECLEN (x%d, 0) == %d", depth, p->u.veclen);
      break;

    case DT_elt_zero_int:
      printf ("XINT (x%d, 0) == %d", depth, (int) p->u.intval);
      break;

    case DT_elt_one_int:
      printf ("XINT (x%d, 1) == %d", depth, (int) p->u.intval);
      break;

    case DT_elt_zero_wide:
    case DT_elt_zero_wide_safe:
      printf ("XWINT (x%d, 0) == ", depth);
      print_host_wide_int (p->u.intval);
      break;

    case DT_const_int:
      printf ("x%d == const_int_rtx[MAX_SAVED_CONST_INT + (%d)]",
       depth, (int) p->u.intval);
      break;

    case DT_veclen_ge:
      printf ("XVECLEN (x%d, 0) >= %d", depth, p->u.veclen);
      break;

    case DT_dup:
      printf ("rtx_equal_p (x%d, operands[%d])", depth, p->u.dup);
      break;

    case DT_pred:
      printf ("%s (x%d)", p->u.pred.name, depth);
      break;

    case DT_c_test:
      print_c_condition (p->u.c_test);
      break;

    case DT_accept_insn:
      ((void)(__builtin_expect(!(subroutine_type == RECOG), 0) ? __builtin_unreachable(), 0 : 0));
      ((void)(__builtin_expect(!(p->u.insn.num_clobbers_to_add), 0) ? __builtin_unreachable(), 0 : 0));
      printf ("pnum_clobbers != NULL");
      break;

    default:
      __builtin_unreachable();
    }
}

/* { dg-final { scan-assembler "printf" } } */
