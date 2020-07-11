/* PR middle-end/93488
 
   Ensure that wait and async arguments can be cast to the correct type
   without breaking gimple verification.  */

void test()
{
  /* int */ unsigned char a = 1;
  /* int */ unsigned char w = 1;

#pragma acc parallel wait(w) async(a)
  ;
#pragma acc kernels wait(w) async(a)
  ;
#pragma acc serial wait(w) async(a)
  ;
  int data = 0;
#pragma acc enter data wait(w) async(a) create(data)
#pragma acc update wait(w) async(a) device(data)
#pragma acc exit data wait(w) async(a) delete(data)
#pragma acc wait(w) async(a)
}
