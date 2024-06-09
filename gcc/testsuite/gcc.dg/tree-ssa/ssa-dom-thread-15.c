/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-dom2-details -w" } */
struct rtx_def;
typedef struct rtx_def *rtx;
struct machine_frame_state
{
  rtx cfa_reg;
  long sp_offset;
};
struct machine_function {
  struct machine_frame_state fs;
};
enum global_rtl_index
{
  GR_PC,
  GR_CC0,
  GR_RETURN,
  GR_SIMPLE_RETURN,
  GR_STACK_POINTER,
  GR_FRAME_POINTER,
  GR_HARD_FRAME_POINTER,
  GR_ARG_POINTER,
  GR_VIRTUAL_INCOMING_ARGS,
  GR_VIRTUAL_STACK_ARGS,
  GR_VIRTUAL_STACK_DYNAMIC,
  GR_VIRTUAL_OUTGOING_ARGS,
  GR_VIRTUAL_CFA,
  GR_VIRTUAL_PREFERRED_STACK_BOUNDARY,
  GR_MAX
};
struct target_rtl {
  rtx x_global_rtl[GR_MAX];
};
extern struct target_rtl default_target_rtl;
struct function {
  struct machine_function * machine;
};
extern struct function *cfun;
struct ix86_frame
{
  long stack_pointer_offset;
};
int ix86_target_stack_probe (void);
int pro_epilogue_adjust_stack (rtx, rtx, rtx, int, int);
rtx gen_rtx_CONST_INT (int);
void fancy_abort (const char *, int, const char *);
void
ix86_expand_prologue (void)
{
  struct machine_function *m = (cfun + 0)->machine;
  struct ix86_frame frame;
  long allocate;
  allocate = frame.stack_pointer_offset - m->fs.sp_offset;
  if (allocate == 0)
    ;
  else if (!ix86_target_stack_probe ()) 
    {
      pro_epilogue_adjust_stack ((((&default_target_rtl)->x_global_rtl)[GR_STACK_POINTER]), (((&default_target_rtl)->x_global_rtl)[GR_STACK_POINTER]),
            gen_rtx_CONST_INT ((-allocate)), -1,
            m->fs.cfa_reg == (((&default_target_rtl)->x_global_rtl)[GR_STACK_POINTER]));
    }
  ((void)(!(m->fs.sp_offset == frame.stack_pointer_offset) ? fancy_abort ("../../gcc-4.7.3/gcc/config/i386/i386.c", 10435, __FUNCTION__), 0 : 0));
}

/* In the case where ALLOCATE is zero, we know that sp_offset and
   stack_poitner_offset within their respective structures are the
   same.  That allows us to thread the jump from the true arm of the
   first IF conditional around the test controlling the call to
   fancy_abort.  */
/* { dg-final { scan-tree-dump-times "Threaded" 1 "dom2"} } */

