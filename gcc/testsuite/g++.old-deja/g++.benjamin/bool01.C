//980323 bkoz
//test for bools with inclusive ors

#include <assert.h>
#include <limits.h>

void bar ( bool  x ) {};
void bars ( short  x ) {};

/* 980326 bkoz this is not initialized and so can have indeterminate value. */
#if 0
int orb(){
  bool y;
  bar ( y );
  int blob = ( 27 | int (y) );
  return blob; //expect 27 or 0
}
#endif

int orbtrue(){
  bool y = true;
  bar ( y );
  int blob = ( 27 | int (y) );
  return blob; //expect 27
}

int orbfalse(){
  bool y = false;
  bar ( y );
  int blob = ( 27 | int (y) );
  return blob; //expect 27
}

int orbfalse2(){
  bool y = 0;
  bar ( y );
  int blob = ( 27 | int (y) );
  return blob;  //expect 27
}

int ors(){
  short y = 1;
  bars ( y );
  int blob = ( 27 | int (y) );
  return blob;  //expect 27
}


#if INT_MAX > 32767
int orus(){
  unsigned short y = 1;
  bars ( y );
  int blob = ( 65539 | int (y) );
  return blob;  //expect 65539, will be 3 if done in us type
}
#endif

int main() {
  int tmp;
#if 0
  tmp = orb();
  assert (tmp == 27 || tmp == 0);
#endif
  tmp = orbtrue();
  assert (tmp ==27);
  tmp = orbfalse();
  assert (tmp ==27);
  tmp = orbfalse2();
  assert (tmp ==27);
  tmp = ors();
  assert (tmp ==27);
#if INT_MAX > 32767
  tmp = orus();
  assert (tmp == 65539);
#endif

  return 0;
}

