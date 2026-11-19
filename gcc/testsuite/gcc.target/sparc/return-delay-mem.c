/* PR target/128395 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-stack-protector" } */

typedef unsigned long long u64;
struct cputimer { u64 utime, stime, sum_exec; };
struct pct { int timers_active; };
struct sig { struct cputimer cputimer; struct pct pct; };
struct task { struct sig *signal; };

void start_sample (struct task *, u64 *);
void group_sample (struct task *, u64 *);

static inline void
proc_sample (struct cputimer *c, u64 *samples)
{
  samples[0] = c->utime + c->stime;
  samples[1] = c->utime;
  samples[2] = c->sum_exec;
}

u64
sample_group (const int clkid, struct task *p, _Bool start)
{
  struct cputimer *cputimer = &p->signal->cputimer;
  struct pct *pct = &p->signal->pct;
  u64 samples[3];

  if (!*(volatile int *) &pct->timers_active)
    {
      if (start)
       start_sample (p, samples);
      else
       group_sample (p, samples);
    }
  else
    proc_sample (cputimer, samples);

  return samples[clkid];
}

double fill (double *);

double
fp_sample (int i)
{
  double v[3];

  fill (v);
  return v[i];
}

/* { dg-final { scan-assembler-not "return\[ \t\]+%i7\\+8\[ \t\]*\n\[ \t\]+ld" } } */
