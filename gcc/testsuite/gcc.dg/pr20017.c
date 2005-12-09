/* PR rtl-optimization/20017

   After CSE/GCSE folds a switch statement to an unconditional jump,
   cfg_cleanup did not remove a dead jump table, confusing the CFG
   layout code later on.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */
/* { dg-options "-O1 -march=i386" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

int
foo (int *buf, int *p)
{
  int result;
  const int *tmp;

  if (*buf)
    return 1;

  result = 2;
  *buf = 2;
  tmp = buf;
  switch (*tmp)
    {
    case 3:
    case 4:
    case 6:
    case 14:
      return 1;

    case 0:
      result = *p;

      /* Fall through.  */
    default:
      if (result)
	return 1;
    }

  return 0;
}
