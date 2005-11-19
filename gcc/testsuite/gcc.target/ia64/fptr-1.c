/* { dg-do run { target ia64-*-linux* } } */
/* { dg-options "-O2" } */

/* Test function descriptor access.  */

extern unsigned long *_GLOBAL_OFFSET_TABLE_;
extern void abort(void);

struct ia64_fdesc
{
  unsigned long func;
  unsigned long gp;
};

void
os_boot_rendez (void)
{
}

static int
check (unsigned long gp)
{
  return gp != (unsigned long) &_GLOBAL_OFFSET_TABLE_;
}

int
main (int argc, char **argv)
{
  int i;
  int res = 0;

  for (i = 0; i < 1; i++)
    res += check (((struct ia64_fdesc *) os_boot_rendez)->gp);
  if (res)
    abort ();
  return res;
}
