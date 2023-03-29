#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>


int func (int code, ...)
{
  va_list argsPtr;
  int i, v;

  va_start(argsPtr, code);
  for (i=1; i<=code; i++) {
    v = va_arg(argsPtr, int);
    printf("%d parameter is %d\n", i, v);
    if (i != v)
      exit(1);
  }
  va_end(argsPtr);
  return 1;
}


int funcptr (int code, ...)
{
  va_list argsPtr;
  int l, v;
  char *p;

  va_start(argsPtr, code);
  switch (code) {

  case 1:   
    p = va_arg(argsPtr, char *);
    l = va_arg(argsPtr, int);
    printf("parameter is %s and length %d\n", p, l);
    if (strlen(p) != l)
      exit(1);
    break;
  }
  va_end(argsPtr);
  return 1;
}


int funcptrint (char *p, ...)
{
  va_list argsPtr;
  int l, v;

  va_start(argsPtr, p);

  l = va_arg(argsPtr, int);
  printf("parameter is %s and length %d\n", p, l);
  if (strlen(p) != l)
    exit(1);

  va_end(argsPtr);
  return 1;
}
