/* { dg-do run { target cet } } */
/* { dg-options "-O2 -fcf-protection -mcet" } */

void _exit(int status) __attribute__ ((__noreturn__));

#ifdef __x86_64__
# define incssp(x) __builtin_ia32_incsspq (x)
# define rdssp(x) __builtin_ia32_rdsspq (x)
#else
# define incssp(x) __builtin_ia32_incsspd (x)
# define rdssp(x) __builtin_ia32_rdsspd (x)
#endif

static void
__attribute__ ((noinline, noclone))
test (unsigned long frames)
{
  unsigned long ssp = 0;
  ssp = rdssp (ssp);
  if (ssp != 0)
    {
      unsigned long tmp = frames;
      while (tmp > 255)
	{
	  incssp (tmp);
	  tmp -= 255;
	}
      incssp (tmp);
    }
  /* We must call _exit since shadow stack is incorrect now.  */
  _exit (0);
}

int
main ()
{
  test (1);
  return 0;
}
