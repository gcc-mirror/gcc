/* { dg-do compile } */

struct _Unwind_Context
{
  void *reg[17];
  void *ra;
};
extern void bar (struct _Unwind_Context *);
void
__frame_state_for (void *pc_target)
{
  struct _Unwind_Context context;
  __builtin_memset (&context, 0, sizeof (struct _Unwind_Context));
  context.ra = pc_target;
  bar (&context);
}
