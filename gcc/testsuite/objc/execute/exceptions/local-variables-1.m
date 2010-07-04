/* Check that local variables that get modified inside the @try
   block survive until the @catch block is reached.  */
/* Developed by Ziemowit Laski <zlaski@apple.com>.  */

#include <stdlib.h>
#include <stdio.h>
#import "../../../objc-obj-c++-shared/Object1.h"

int gi1 = 9, gi2 = 19;
float gf1 = 9.0, gf2 = 19.0;
id obj2 = nil;

void foo (int arg1, float *arg2)
{
  int *pi = &gi1;
  float *pf = &gf1;
  id obj1 = nil;
  int local1 = 45, local2 = 47;
  float local3 = 3.0, local4 = 4.0;
  register int local5 = 15;
  static float local6 = 16.0;

  @try {
    local1 = 123;
    local2 = 345;
    local3 = 5.0;
    local4 = 6.0;
    local5 = 17;
    local6 = 18.0;
    pi = &gi2;
    pf = &gf2;
    obj2 = obj1 = [Object new];
    arg1 = 17;
    arg2 = &gf2;
    
    @throw [Object new];
  }
  @catch (Object *obj) {
    if (local1 != 123 || local2 != 345 || local3 != 5.0 || local4 != 6.0
       || local5 != 17 || local6 != 18.0) {
      printf("Abort 1\n");
      abort();
    }
    if(pi != &gi2 || pf != &gf2) {
      printf("Abort 2\n");
      abort();
    }
    if(!obj1 || obj1 != obj2) {
      printf("Abort 3\n");
      abort();
    }
    if(arg1 != 17 || arg2 != &gf2) {
      printf("Abort 4\n");
      abort();
    }
  }
}

int main(void) {
  foo(15, &gf1);
  return 0;
}
