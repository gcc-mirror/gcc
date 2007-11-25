/* Check BIND(C) for ENTRY
   PR fortran/34079
   To be linked with bind_c_usage_10.f03
*/

void mySub1(int *);
void mySub3(int *);
void mySubEnt2(float *);
void mySubEnt3(float *);
void sub4ent(float *);

int myFunc1(void);
int myFunc3(void);
float myFuncEnt2(void);
float myFuncEnt3(void);
float func4ent(void);

extern void abort(void);

int main()
{
  int i = -1;
  float r = -3.0f;

  mySub1(&i);
  if(i != 5) abort();
  mySub3(&i);
  if(i != 7) abort();
  mySubEnt2(&r);
  if(r != 66.0f) abort();
  mySubEnt3(&r);
  if(r != 77.0f) abort();
  sub4ent(&r);
  if(r != 88.0f) abort();

  i = myFunc1();
  if(i != -5) abort();
  i = myFunc3();
  if(i != -7) abort();
  r = myFuncEnt2();
  if(r != -66.0f) abort();
  r = myFuncEnt3();
  if(r != -77.0f) abort();
  r = func4ent();
  if(r != -88.0f) abort();

  return 0;
}
