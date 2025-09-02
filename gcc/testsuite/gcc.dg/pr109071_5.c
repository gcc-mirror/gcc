/* PR tree-optimization/109071 need more context for -Warray-bounds warnings
   due to code duplication from jump threading.
   test case is from PR115274, which is a duplication of PR109071.  */  
/* { dg-options "-O2 -Wstringop-overread -fdiagnostics-show-context=1" } */
/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */
#include <string.h>
char *c;
void a(long);
int b(char *d) { return strlen(d); } /* { dg-warning "or more bytes from a region of size 0" } */
void e() {
  long f = 1;
  f = b(c + f);
  if (c == 0)
    a(f);
}
/* { dg-begin-multiline-output "" }
   NN | int b(char *d) { return strlen(d); }
      |                         ^~~~~~~~~
  'e': events 1-2
   NN | int b(char *d) { return strlen(d); }
      |                         ~~~~~~~~~
      |                         |
      |                         (2) warning happens here
......
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN |   if (c == 0)
      |      ^
      |      |
      |      (1) when the condition is evaluated to true
   { dg-end-multiline-output "" } */
