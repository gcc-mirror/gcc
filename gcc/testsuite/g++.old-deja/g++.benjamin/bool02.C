// { dg-do run  }
//980324 bkoz
//test for bool and bitwise ands

#include <assert.h>


void bar ( bool  x ) {}
void bars ( short  x ) {}

#if 0
int andb(){
  bool y;
  bar ( y );
  int blob = ( 27 & int (y) );
  return blob; //expect 1 or 0
}
#endif

int andbtrue(){
  bool y = true;
  bar ( y );
  int blob = ( 27 & int (y) );
  return blob; //expect 1
}

int andbfalse(){
  bool y = false;
  bar ( y );
  int blob = ( 27 & int (y) );
  return blob; //expect 0
}

int andbfalse2(){
  bool y = 0;
  bar ( y );
  int blob = ( 27 & int (y) );
  return blob;  //expect 0
}

int ands(){
  short y = 1;
  bars ( y );
  int blob = ( 27 & int (y) );
  return blob;  //expect 1
}


int main() {
  int tmp;
#if 0
  tmp = andb();
  assert (tmp == 1 || tmp == 0);
#endif
  tmp = andbtrue();
  assert (tmp == 1);
  tmp = andbfalse();
  assert (tmp == 0);
  tmp = andbfalse2();
  assert (tmp == 0);
  tmp = ands();
  assert (tmp == 1);
  return 0;
}
