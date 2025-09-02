/* PR tree-optimization/109071 need more context for -Warray-bounds warnings
   due to code duplication from jump threading.
   test case is from PR108770, which is a duplication of PR109071.  */  
/* { dg-options "-O2 -Warray-bounds -fdiagnostics-show-context=1" } */
/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */
extern void put(int i);
int check_idx(int i) {
  if (i > 1)
    put(i);
  return i;
}
const char *arr[] = {"A", 0};
void init() {
  int i = 0;
  while (arr[check_idx(i)] != 0) { /* { dg-warning "is above array bounds of" } */
    if (arr[check_idx(i)]) {}
    i++;
  }
}
/* { dg-begin-multiline-output "" }
   NN |   while (arr[check_idx(i)] != 0) {
      |          ~~~^~~~~~~~~~~~~~
  'init': events 1-2
   NN |   if (i > 1)
      |      ^
      |      |
      |      (1) when the condition is evaluated to true
......
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN |   while (arr[check_idx(i)] != 0) {
      |          ~~~~~~~~~~~~~~~~~
      |             |
      |             (2) warning happens here
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN | const char *arr[] = {"A", 0};
      |             ^~~
   { dg-end-multiline-output "" } */
