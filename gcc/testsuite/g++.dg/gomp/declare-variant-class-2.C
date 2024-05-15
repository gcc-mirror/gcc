/* { dg-do compile } */
/* { dg-additional-options "-foffload=disable" } */
/* { dg-additional-options "-mavx512bw -mavx512vl" { target { i?86-*-* x86_64-*-* } } } */

/* References to function parameters in dynamic selector expressions for
   "declare variant" isn't supported yet; see PR 113904.  Check to see that
   a proper error is diagnosed meanwhile and GCC doesn't just wander off
   into the weeds and ICE.  */

extern int frob (int);
extern int frobmore (class junk *);

class junk 
{
 public:
  int data;
  void f01 (int, int);
  void f02 (int, int);
  void f03 (int, int);
  void f04 (int, int);
  void f05 (int, int);
#pragma omp declare variant (f01) match (target_device={device_num (devnum), isa("avx512f","avx512vl")}) /* { dg-message "sorry, unimplemented: reference to function parameter" } */
#pragma omp declare variant (f02) match (implementation={vendor(score(15):gnu)})
#pragma omp declare variant (f03) match (user={condition(score(11):frob (ok + 42))}) /* { dg-message "sorry, unimplemented: reference to function parameter" } */
#pragma omp declare variant (f04) match (user={condition(score(11):data)}) /* { dg-message "sorry, unimplemented: reference to function parameter" } */
#pragma omp declare variant (f05) match (user={condition(score(11):frobmore (this))}) /* { dg-message "sorry, unimplemented: reference to function parameter" } */
  void f06 (int devnum, int ok);
};

void
test1 (junk *j)
{
  int i;
  #pragma omp parallel for
  for (i = 0; i < 1; i++)
    j->f06 (17, 1);
}
