/* PR tree-optimization/109071 need more context for -Warray-bounds warnings
   due to code duplication from jump threading.
   test case is from PR88771, which is a duplication of PR109071.  */  
/* { dg-options "-O2 -fdiagnostics-show-context=1" } */
/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */
typedef struct {
  int a;
} * b;

char *c, *x;
int f;

void d() {
  b e;
  char a = f + 1 ?: f;
  __builtin_strncpy(c, x, f); /* { dg-warning "exceeds maximum object size" } */
  if (a)
    e->a = 0;
}
/* { dg-begin-multiline-output "" }
   NN |   __builtin_strncpy(c, x, f);
      |   ^~~~~~~~~~~~~~~~~~~~~~~~~~
  'd': events 1-2
   NN |   char a = f + 1 ?: f;
      |        ^
      |        |
      |        (1) when the condition is evaluated to false
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN |   __builtin_strncpy(c, x, f);
      |   ~~~~~~~~~~~~~~~~~~~~~~~~~~
      |   |
      |   (2) warning happens here
   { dg-end-multiline-output "" } */
