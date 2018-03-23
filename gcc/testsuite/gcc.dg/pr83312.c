/* { dg-options "-O2 -Warray-bounds" } */

struct ptlrpcd_ctl {
  char pc_name[20];
};
struct ptlrpcd {
  struct ptlrpcd_ctl pd_threads[6];
};
struct ptlrpcd *ptlrpcd_init_pd;
static void ptlrpcd_ctl_init(struct ptlrpcd_ctl *pc, int index) {
  if (index < 0)
    __builtin_snprintf(pc->pc_name, sizeof(pc->pc_name), "ptlrpcd_rcv");
  else
    __builtin_snprintf(pc->pc_name, sizeof(pc->pc_name), "ptlrpcd_%d", index);
}
int ptlrpcd_init_ncpts;
static int ptlrpcd_init(int nthreads) {
  int j;
  if (ptlrpcd_init_ncpts) {
    ptlrpcd_ctl_init(&ptlrpcd_init_pd->pd_threads[0], -1);
    for (j = 1; j < nthreads; j++)
      ptlrpcd_ctl_init(&ptlrpcd_init_pd->pd_threads[j], j);
  }
  return 0;
}
int ptlrpcd_init_groupsize;
void ptlrpcd_addref(void) {
    ptlrpcd_init(ptlrpcd_init_groupsize);
}

