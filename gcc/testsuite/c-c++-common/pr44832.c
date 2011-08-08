/* PR debug/44832 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */
/* { dg-options "-O2 -fcompare-debug -fno-short-enums" {target short_enums} } */
/* { dg-require-effective-target int32plus } */

struct rtx_def;
typedef struct rtx_def *rtx;
typedef const struct rtx_def *const_rtx;
struct rtvec_def;
typedef struct rtvec_def *rtvec;
extern int ix86_isa_flags;

enum machine_mode
{
  VOIDmode,
  V8HImode,
  V16QImode,
  V4SImode,
  V2DImode,
  V32QImode,
  MAX_MACHINE_MODE,

  NUM_MACHINE_MODES = MAX_MACHINE_MODE
};
extern unsigned char mode_size[NUM_MACHINE_MODES];
extern const unsigned char mode_inner[NUM_MACHINE_MODES];
extern const unsigned char mode_nunits[NUM_MACHINE_MODES];
enum rtx_code {

CONST_INT ,

CONST_FIXED ,

CONST_DOUBLE

  };
union rtunion_def
{
  rtvec rt_rtvec;
};
typedef union rtunion_def rtunion;
struct rtx_def {

  __extension__ enum rtx_code code: 16;

  __extension__ enum machine_mode mode : 8;

  union u {
    rtunion fld[1];
  } u;
};
struct rtvec_def {
  rtx elem[1];
};
extern int rtx_equal_p (const_rtx, const_rtx);
extern rtx gen_reg_rtx (enum machine_mode);

extern void
ix86_expand_vector_init_concat (enum machine_mode mode,
				rtx target, rtx *ops, int n);

static void
ix86_expand_vector_init_general (unsigned char mmx_ok, enum machine_mode mode,
     rtx target, rtx vals)
{
  rtx ops[32], op0, op1;
  enum machine_mode half_mode = VOIDmode;
  int n, i;

  switch (mode)
    {
    case V4SImode:
    case V2DImode:
      n = mode_nunits[mode];
      ix86_expand_vector_init_concat (mode, target, ops, n);
      return;

    case V32QImode:
      goto half;
half:
{
  typedef int eger;
  if (mode != V4SImode)
 ops[0] = 0;
}
      n = mode_nunits[mode];
      for (i = 0; i < n; i++)
 ops[i] = (((((vals)->u.fld[0]).rt_rtvec))->elem[i]);
      op0 = gen_reg_rtx (VOIDmode);
      return;

    case V16QImode:
      if (!((ix86_isa_flags & (1 << 19)) != 0))
 break;

    case V8HImode:
      if (!((ix86_isa_flags & (1 << 17)) != 0))
 break;

      n = mode_nunits[mode];
      for (i = 0; i < n; i++)
 ops[i] = (((((vals)->u.fld[0]).rt_rtvec))->elem[i]);
      return;

    default:
      ;
    }

    {
      int n_words;

      n_words = ((unsigned short) mode_size[mode]) / 4;

      if (n_words == 4)
   ix86_expand_vector_init_general (0, V4SImode, 0, 0);
    }
}


void
ix86_expand_vector_init (unsigned char mmx_ok, rtx target, rtx vals)
{
  enum machine_mode mode = ((enum machine_mode) (target)->mode);
  enum machine_mode inner_mode = ((enum machine_mode) mode_inner[mode]);
  int n_elts = mode_nunits[mode];
  int n_var = 0, one_var = -1;
  unsigned char all_same = 1, all_const_zero = 1;
  int i;
  rtx x;

  for (i = 0; i < n_elts; ++i)
    {
      x = (((((vals)->u.fld[0]).rt_rtvec))->elem[i]);
      if (!((((enum rtx_code) (x)->code) == CONST_INT)
     || ((enum rtx_code) (x)->code) == CONST_DOUBLE
     || ((enum rtx_code) (x)->code) == CONST_FIXED))
 n_var++, one_var = i;
      else 
 all_const_zero = 0;
      if (i > 0 && !rtx_equal_p (x, (((((vals)->u.fld[0]).rt_rtvec))->elem[0])))
 all_same = 0;
    }


  if (n_var == 0)
    {
      return;
    }

  if (all_same)
    return;

  if (n_var == 1)
    {
      if (all_const_zero)
 return;

    }

  ix86_expand_vector_init_general (mmx_ok, mode, target, vals);
}
