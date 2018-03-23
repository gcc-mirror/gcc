/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not " \[/%\] " "optimized" } } */

/* Testcase submitted for PR81165, with its main function removed as
   it's turned into a compile test.  We want to make sure that all of
   the divide/remainder computations are removed by tree optimizers.

   We can figure out that we don't need to compute at runtime even the
   condition to enter the loop: the initial i==0 would have to be
   greater than the sum of two small unsigned values: 1U>>t1 is in the
   range 0..1, whereas the char value is bounded by the range 0..127,
   being 128 % a positive number (zero would invoke undefined
   behavior, so we can assume it doesn't happen).  (We know it's
   nonnegative because it's 10 times a number that has no more than
   the bits for 16, 8 and 1 set.)

   We don't realize that the loop is useless right away: jump
   threading helps remove some of the complexity, particularly of the
   computation within the loop: t1 is compared with 1, but it can
   never be 1.  (We could assume as much, since its being 1 would
   divide by zero, but we don't.)

   If we don't enter the conditional block, t1 remains at 2; if we do,
   it's set to either -1.  If we jump thread at the end of the
   conditional block, we can figure out the ranges exclude 1 and the
   jump body is completely optimized out.  However, we used to fail to
   consider the block for jump threading due to the amount of
   computation in it, without realizing most of it would die in
   consequence of the threading.

   We now take the dying code into account when deciding whether or
   not to try jump threading.  That might enable us to optimize the
   function into { if (x2 != 0 || (x1 & 1) == 0) abort (); }.  At the
   time of this writing, with the patch, we get close, but the test on
   x2 only gets as far as ((1 >> x2) == 0).  Without the patch, some
   of the loop remains.  */

short x0 = 15;

void func (){
  volatile int x1 = 1U;
  volatile char x2 = 0;
  char t0 = 0;
  unsigned long t1 = 2LU;
  int i = 0;
  
  if(1>>x2) {
    t0 = -1;
    t1 = (1&(short)(x1^8U))-1;
  }

  while(i > (int)((1U>>t1)+(char)(128%(10*(25LU&(29%x0)))))) {
    i += (int)(12L/(1!=(int)t1));
  }

  if (t0 != -1) __builtin_abort();
  if (t1 != 0L) __builtin_abort();
}
