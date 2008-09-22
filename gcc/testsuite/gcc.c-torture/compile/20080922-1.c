typedef struct rtx_def *rtx;
typedef struct rtvec_def *rtvec;
enum rtx_code { PARALLEL, SET };
union rtunion_def {
    rtx rt_rtx;
    rtvec rt_rtvec;
};
typedef union rtunion_def rtunion;
struct rtx_def {
    rtunion fld;
};
struct rtvec_def {
    int num_elem;
};
extern rtx operand;

rtx peephole2_insns (rtx x0, enum rtx_code code)
{
  switch (code)
    {
      case SET:
	  operand = (((x0)->fld).rt_rtx);
	  return operand;
      case PARALLEL:
	  if ((((((x0)->fld).rt_rtvec))->num_elem) == 2)
	    return 0;
	  break;
    }
}
