/* Test if #line commands are generated properly even when header
   includes self.  */
/* { dg-do compile } */
#include "20000628-1.h"
int main(void) { return a + b + c; }
