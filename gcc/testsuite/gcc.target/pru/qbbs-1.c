/* Test QBBS recognition */

/* { dg-do run } */
/* { dg-options "-O1" } */

/* -O1 in the options is significant.  Without it bit-check-and-branch
   operation may not be optimized to QBBS.  */

extern void abort (void);

unsigned int
test_qbbs_reg (unsigned int a, unsigned int b, unsigned int val)
{
  if (val & (1 << 19))
    return a;
  return b;
}

int
main (int argc, char** argv)
{
  if (test_qbbs_reg (101, 505, (1u << 19)) != 101)
    abort();
  if (test_qbbs_reg (101, 505, (1u << 18)) != 505)
    abort();

  return 0;
}

