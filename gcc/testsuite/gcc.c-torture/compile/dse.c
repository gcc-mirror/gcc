/* { dg-additional-options "-fpermissive" } */

typedef unsigned long microblaze_reg_t;
struct pt_regs
{
  microblaze_reg_t msr;
  int pt_mode;
};
struct task_struct
{
  void *stack;
};
int
copy_thread (struct task_struct *p)
{
  struct pt_regs *childregs =
    (((struct pt_regs *) ((1 << 13) + ((void *) (p)->stack))) - 1);
  memset (childregs, 0, sizeof (struct pt_regs));
  childregs->pt_mode = 1;
}

