/* { dg-require-effective-target label_values } */
/* { dg-require-effective-target indirect_jumps } */

void
f (void)
{
  long *sp;
  long *pc;

  static void *dummy[] =
    {
      &&L1,
      &&L2,
    };

 L1:
  {
    float val;
    val = *(float *) sp;
    val = -val;
    *(float *) sp = val;
    goto *pc++;
  }

 L2:
  {
    float from;
    *(long long *) sp = from;
    goto *pc++;
  }
}
