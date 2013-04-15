/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1" } */

struct rtx_def;
typedef struct rtx_def *rtx;
typedef const struct rtx_def *const_rtx;
enum machine_mode
{
  MAX_MACHINE_MODE,
  NUM_MACHINE_MODES = MAX_MACHINE_MODE
};
extern const char *const mode_name[NUM_MACHINE_MODES];
enum mode_class
{ MODE_RANDOM, MODE_CC, MODE_INT, MODE_PARTIAL_INT, MODE_FRACT, MODE_UFRACT,
    MODE_ACCUM, MODE_UACCUM, MODE_FLOAT, MODE_DECIMAL_FLOAT, MODE_COMPLEX_INT,
    MODE_COMPLEX_FLOAT, MODE_VECTOR_INT, MODE_VECTOR_FRACT,
    MODE_VECTOR_UFRACT, MODE_VECTOR_ACCUM, MODE_VECTOR_UACCUM,
    MODE_VECTOR_FLOAT, MAX_MODE_CLASS };
extern const unsigned char mode_class[NUM_MACHINE_MODES];
extern const unsigned short mode_precision[NUM_MACHINE_MODES];
struct rtx_def
{
  __extension__ enum machine_mode mode:8;
};
void
convert_move (rtx to, rtx from, int unsignedp)
{
  enum machine_mode to_mode = ((enum machine_mode) (to)->mode);
  enum machine_mode from_mode = ((enum machine_mode) (from)->mode);
  ((void)
   (!((mode_precision[from_mode] != mode_precision[to_mode])
      || ((((enum mode_class) mode_class[from_mode]) == MODE_DECIMAL_FLOAT) !=
	  (((enum mode_class) mode_class[to_mode]) ==
	   MODE_DECIMAL_FLOAT))) ?
    fancy_abort ("/home/gcc/virgin-gcc/gcc/expr.c", 380, __FUNCTION__),
    0 : 0));
}

/* { dg-final { scan-tree-dump "Replaced.*!=.*with.*!=.* " "forwprop1"} } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */



