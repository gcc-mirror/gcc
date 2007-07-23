#include <stdio.h>

void sub0(void);
void c_sub0(void (*sub)(void));
void c_sub1(int (*func)(int));

extern void abort(void);

int main(int argc, char **argv)
{
  printf("hello from C main\n");
  
  sub0();
  return 0;
}

void c_sub0(void (*sub)(void))
{
  printf("hello from c_sub0\n");
  sub();
  
  return;
}

void c_sub1(int (*func)(int))
{
  int retval;
  
  printf("hello from c_sub1\n");

  retval = func(10);
  if(retval != 10)
  {
    fprintf(stderr, "Fortran function did not return expected value!\n");
    abort();
  }

  return;
}
