extern void abort (void);

typedef unsigned long HARD_REG_SET[2];
HARD_REG_SET reg_class_contents[2];

struct du_chain
{
  struct du_chain *next_use;
  int cl;
};

void __attribute__((noinline))
merge_overlapping_regs (HARD_REG_SET *p)
{
  if ((*p)[0] != -1 || (*p)[1] != -1)
    abort ();
}

void __attribute__((noinline))
regrename_optimize (struct du_chain *this)
{
  HARD_REG_SET this_unavailable;
  unsigned long *scan_fp_;
  int n_uses;
  struct du_chain *last;

  this_unavailable[0] = 0;
  this_unavailable[1] = 0;

  n_uses = 0;
  for (last = this; last->next_use; last = last->next_use)
    {
      scan_fp_ = reg_class_contents[last->cl];
      n_uses++;
      this_unavailable[0] |= ~ scan_fp_[0];
      this_unavailable[1] |= ~ scan_fp_[1];
    }
  if (n_uses < 1)
    return;

  scan_fp_ = reg_class_contents[last->cl];
  this_unavailable[0] |= ~ scan_fp_[0];
  this_unavailable[1] |= ~ scan_fp_[1];

  merge_overlapping_regs (&this_unavailable);
}

int main()
{
  struct du_chain du1 = { 0, 0 };
  struct du_chain du0 = { &du1, 1 };
  reg_class_contents[0][0] = -1;
  reg_class_contents[0][1] = -1;
  reg_class_contents[1][0] = 0;
  reg_class_contents[1][1] = 0;
  regrename_optimize (&du0);
  return 0;
}
