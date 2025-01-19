/* { dg-do compile } */
/* { dg-additional-options "-foffload=disable" } */
/* { dg-additional-options "-mavx512bw -mavx512vl" { target { i?86-*-* x86_64-*-* } } } */

/* References to function parameters in dynamic selector expressions for
   "declare variant" isn't supported yet; see PR 113904.  Check to see that
   a proper error is diagnosed meanwhile and GCC doesn't just wander off
   into the weeds and ICE.  */

extern int frob (int);

class junk 
{
 public:
  int data;
  static void f01 (int, int);
  static void f02 (int, int);
  static void f03 (int, int);
#pragma omp declare variant (f01) match (target_device={device_num (devnum), isa("avx512f","avx512vl")}) /* { dg-message "sorry, unimplemented: reference to function parameter" } */
#pragma omp declare variant (f02) match (implementation={vendor(score(15):gnu)})
#pragma omp declare variant (f03) match (user={condition(score(11):frob (ok + 42))}) /* { dg-message "sorry, unimplemented: reference to function parameter" } */
  static void f04 (int devnum, int ok);
};

void
test1 (void)
{
  int i;
  #pragma omp parallel for
  for (i = 0; i < 1; i++)
    junk::f04 (17, 1);
}
