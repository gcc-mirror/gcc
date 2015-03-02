/* { dg-lto-do link } */
/* { dg-lto-options { { -flto -O1 -fdevirtualize } } } */
extern void fn3 (void); 

void fn2(void) 
{ 
  fn3(); 
}

void fn1(void) 
{ 
  fn2(); 
}

void fn4(void) 
{ 
  fn2(); 
}

