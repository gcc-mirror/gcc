/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection" } */
/* { dg-require-ifunc "" } */
/* { dg-final { scan-assembler-times {\mendbr} 4 } } */

int resolver_fn = 0;
int resolved_fn = 0;

static inline void
do_it_right_at_runtime_A (void)
{
  resolved_fn++;
}

static inline void
do_it_right_at_runtime_B (void)
{
  resolved_fn++;
}

static inline void do_it_right_at_runtime (void);

void do_it_right_at_runtime (void)
  __attribute__ ((ifunc ("resolve_do_it_right_at_runtime")));

extern int r;
static void (*resolve_do_it_right_at_runtime (void)) (void)
{
  resolver_fn++;

  typeof(do_it_right_at_runtime) *func;
  if (r & 1)
    func = do_it_right_at_runtime_A;
  else
    func = do_it_right_at_runtime_B;

  return (void *) func;
}

int
main ()
{
  do_it_right_at_runtime ();
  return 0;
}
