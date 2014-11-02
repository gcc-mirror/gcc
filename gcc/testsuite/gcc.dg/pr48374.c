/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O -fschedule-insns2 -fsel-sched-pipelining -fsel-sched-pipelining-outer-loops -fselective-scheduling2 --param max-sched-extend-regions-iters=2"  } */

void foo (int y)
{
  switch (y)
    {
    case 3:
    case 5:
    case 7:
    case 11:
      break;
    default:
      __builtin_unreachable ();
    }
}

