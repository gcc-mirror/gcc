#include <stdio.h>
#include <objc/NXConstStr.h>

int main(int argc, void **args)
{
  printf ([@"this is a string\n" cString]);
  return 0;
}
