/* Check that the SH specific sh_treg_combine RTL optimization pass works as
   expected.  */
/* { dg-do compile }  */
/* { dg-options "-O2 -fno-tree-dominator-opts -fno-tree-vrp" } */

/* { dg-final { scan-assembler-not "not\t" } } */
/* { dg-final { scan-assembler-times "cmp/eq" 2 } } */
/* { dg-final { scan-assembler-times "cmp/hi" 4 } } */
/* { dg-final { scan-assembler-times "cmp/gt" 2 } } */

/* { dg-final { scan-assembler-times "tst" 7 { target { ! sh2a } } } } */
/* { dg-final { scan-assembler-times "movt" 2 { target { ! sh2a } } } } */

/* { dg-final { scan-assembler-times "tst" 6 { target { sh2a } } } } */
/* { dg-final { scan-assembler-not "movt" { target { sh2a } } } } */
/* { dg-final { scan-assembler-times "nott" 2 { target { sh2a } } } } */


/* non-SH2A: 2x tst, 1x movt, 2x cmp/eq, 1x cmp/hi
   SH2A: 1x tst, 1x nott, 2x cmp/eq, 1x cmp/hi  */
static inline int
blk_oversized_queue_0 (int* q)
{
  if (q[2])
    return q[1] == 5; 
  return (q[0] != 5);
}

int __attribute__ ((noinline))
get_request_0 (int* q, int rw)
{
  if (blk_oversized_queue_0 (q))
    {
      if ((rw == 1) || (rw == 0))
	return -33;
      return 0;
    }
  return -100;
}


/* 1x tst, 1x cmp/gt, 1x cmp/hi
   On SH2A mem loads/stores have a wrong length of 4 bytes and thus will
   not be placed in a delay slot.  This introduces an extra cmp/gt insn.  */
static inline int
blk_oversized_queue_1 (int* q)
{
  if (q[2])
    return q[1] > 5; 
  return (q[0] > 5);
}

int __attribute__ ((noinline))
get_request_1 (int* q, int rw)
{
  if (blk_oversized_queue_1 (q))
    {
      if ((rw == 1) || (rw == 0))
	return -33;
      return 0;
    }
  return -100;
}


/* 1x tst, 1x cmp/gt, 1x cmp/hi, 1x cmp/hi  */
static inline int
blk_oversized_queue_2 (int* q)
{
  if (q[2])
    return q[1] > 5; 
  return (q[0] < 5);
}

int __attribute__ ((noinline))
get_request_2 (int* q, int rw)
{
  if (blk_oversized_queue_2 (q))
    {
      if ((rw == 1) || (rw == 0))
	return -33;
      return 0;
    }
  return -100;
}


/* 3x tst, 1x movt, 1x cmp/hi, 1x not  */
static inline int
blk_oversized_queue_5 (int* q)
{
  if (q[2])
    return q[1] != 0; 
  return q[0] == 0;
}

int __attribute__ ((noinline))
get_request_5 (int* q, int rw)
{
  if (blk_oversized_queue_5 (q))
    {
      if ((rw == 1) || (rw == 0))
	return -33;
      return 0;
    }
  return -100;
}
