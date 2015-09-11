/* { dg-options "-O2 -fdevirtualize" } */
extern void fn1(void);
extern void fn4 (void); 

int a; 

void fn3(void) 
{
  for (; a;)
    fn4();
}

int main() {
  fn1();
  return 0;
}

