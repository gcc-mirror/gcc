/* PR tree-optimization/109071 need more context for -Warray-bounds warnings
   due to compiler optimization.  */
/* { dg-options "-O2 -Warray-bounds -fdiagnostics-show-context=1" } */
/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */
#define MAX_LENGTH 10
int a[MAX_LENGTH];

void __attribute__ ((noinline))  foo (int i)
{
  if (i == 12)
    a[i] = -1; /* { dg-warning "is above array bounds of" } */
  else
    a[i] = i;
}

int main ()
{
  for (int i = 0; i < MAX_LENGTH; i++)
    foo (i);
  return 0;
}

/* { dg-begin-multiline-output "" }
   NN |     a[i] = -1;
      |     ~^~~
  'foo': events 1-2
   NN |   if (i == 12)
      |      ^
      |      |
      |      (1) when the condition is evaluated to true
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN |     a[i] = -1;
      |     ~~~~
      |      |
      |      (2) warning happens here
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN | int a[MAX_LENGTH];
      |     ^
   { dg-end-multiline-output "" } */
