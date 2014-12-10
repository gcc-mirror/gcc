/* { dg-do compile } */
/* { dg-options "-Os -fomit-frame-pointer" } */

/* Caused an ICE because of forgotten auto increment.  */

register void *current __asm__("%a2");

struct kernel_stat
{
 long long user;
 long long nice;
 long long system;
 long long idle;
 long long steal;
 unsigned irqs[256];
};
extern struct kernel_stat per_cpu__kstat;
void seq_printf ();

void show_stat(void)
{
  int i;
  long long user, nice, system, idle, steal;
  long long sum = 0;

  user = nice = system = idle = steal = 0;
  for (i = 0; i < 1; i++)
    {
      int j;
      user = user + per_cpu__kstat.user;
      nice = nice + per_cpu__kstat.nice;
      system = system + per_cpu__kstat.system;
      idle = idle + per_cpu__kstat.idle;
      steal = steal + per_cpu__kstat.steal;

      for (j = 0 ; j < 256 ; j++)
	sum += per_cpu__kstat.irqs[j];
    }
  seq_printf(user, nice, system, idle, steal);
  seq_printf(sum);
  for (i = 0; i < 256; i++)
    seq_printf (i);
}
