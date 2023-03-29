#include <stdlib.h>
#include <unistd.h>
#include <setjmp.h>


void func (void)
{
  int r;
  int env[100];
  //  sigjmp_buf env;

  r = __builtin_setjmp(env);
  __builtin_exit (r);
}

