/* { dg-options "-fexceptions -I." } */
/* { dg-require-effective-target exceptions } */
#include "except-1.h"

int main(void) 
{
  return foo(1);
}
