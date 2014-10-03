/* { dg-do compile } */

typedef struct rtx_def *rtx;
enum machine_mode {
  VOIDmode,
  CCFPmode,
  CCFPUmode,
  MAX_MACHINE_MODE
};
enum mode_class {
  MODE_CC,
  MODE_FLOAT,
  MODE_COMPLEX_FLOAT,
  MODE_VECTOR_FLOAT
};
extern const enum mode_class mode_class[(int) MAX_MACHINE_MODE];
enum rtx_code {
  UNKNOWN,
  GEU,
  ORDERED,
  CONST_INT
};
struct rtx_def {
  unsigned int code: 16;
  unsigned int mode : 8;
};
extern enum rtx_code reverse_condition (enum rtx_code);
extern enum rtx_code reverse_condition_maybe_unordered (enum rtx_code);
enum rtx_code
reversed_comparison_code_parts (enum rtx_code code, rtx insn, rtx arg0,
				rtx arg1)
{
  enum machine_mode mode;
  mode = (enum machine_mode) (arg0)->mode;
  if (mode == VOIDmode)
    mode = (enum machine_mode) (arg1)->mode;
  if ((mode_class[(int) (mode)]) == MODE_CC)
    return (mode != CCFPmode && mode != CCFPUmode
	    ? reverse_condition (code)
	    : reverse_condition_maybe_unordered (code));
  switch (code) 
    {
    case GEU:
      return reverse_condition (code);
    case ORDERED:
      return UNKNOWN;
    }
  if (((enum rtx_code) (arg0)->code) == CONST_INT
      || (((enum machine_mode) (arg0)->mode) != VOIDmode
	  && ! ((mode_class[(int) (mode)]) == MODE_FLOAT
		|| (mode_class[(int) (mode)]) == MODE_COMPLEX_FLOAT
		|| (mode_class[(int) (mode)]) == MODE_VECTOR_FLOAT)))
    return reverse_condition (code);
  return UNKNOWN;
}
