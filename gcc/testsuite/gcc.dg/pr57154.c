/* { dg-do compile } */
/* { dg-options "-O2 -fschedule-insns" } */
/* { dg-require-effective-target scheduling } */

#define PF_FROZEN 0x00010000
#define likely(x)      __builtin_expect(!!(x), 1)

struct cur
{
  unsigned long flags;
};
struct cur *cur;

unsigned long freeze_cnt;

extern int foo(void *);
extern int slow_path(void *);

static inline int freezing(void *p)
{
        if (likely(!foo(&freeze_cnt)))
                return 0;
        return slow_path(p);
}

extern int blah(void);

int testcase(int check_kthr_stop)
{
  int was_frozen = 0;

  for (;;) {
    if (!freezing(cur) ||
        (check_kthr_stop && blah()))
      cur->flags &= ~PF_FROZEN;

    if (!(cur->flags & PF_FROZEN))
      break;

    was_frozen = 1;
  }

  return was_frozen;
}
