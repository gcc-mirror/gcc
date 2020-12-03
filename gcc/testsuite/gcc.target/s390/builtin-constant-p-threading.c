/* { dg-do compile } */
/* { dg-options "-O2 -march=z196 -mzarch" } */

typedef struct
{
  int counter;
} atomic_t;

static inline __attribute__ ((__gnu_inline__)) int
__atomic_add (int val, int *ptr)
{
  int old;
  asm volatile("laa %[old],%[val],%[ptr]\n"
	       : [old] "=d" (old), [ptr] "+Q"(*ptr)
	       : [val] "d" (val)
	       : "cc", "memory");
  return old;
}

static inline __attribute__ ((__gnu_inline__)) void
__atomic_add_const (int val, int *ptr)
{
  asm volatile("asi %[ptr],%[val]\n"
	       : [ptr] "+Q" (*ptr)
	       : [val] "i" (val)
	       : "cc", "memory");
}

static inline __attribute__ ((__gnu_inline__)) void
atomic_add (int i, atomic_t *v)
{
  if (__builtin_constant_p (i) && (i > -129) && (i < 128))
    {
      __atomic_add_const (i, &v->counter);
      return;
    }
  __atomic_add (i, &v->counter);
}

static atomic_t num_active_cpus = { (0) };

void
ledtrig_cpu (_Bool is_active)
{
  atomic_add (is_active ? 1 : -1, &num_active_cpus);
}
