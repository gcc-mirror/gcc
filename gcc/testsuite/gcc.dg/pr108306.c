/* { dg-do compile } */
/* { dg-options "-O2  -fno-strict-overflow -fsanitize=shift -Warray-bounds" } */

enum psi_task_count {
	NR_IOWAIT,
	NR_PSI_TASK_COUNTS = 4,
};

unsigned int tasks[NR_PSI_TASK_COUNTS];

static void psi_group_change(unsigned int set)
{
	unsigned int t;
	unsigned int state_mask = 0;

	for (t = 0; set; set &= ~(1 << t), t++)
		if (set & (1 << t))
			tasks[t]++;
}

void psi_task_switch(int sleep)
{
	int set = 0;

	if (sleep)
		set |= (1 << NR_IOWAIT);

	psi_group_change(set);
}
