/* PR tree-optimization/109071 need more context for -Warray-bounds warnings
   due to compiler optimization.  */
/* { dg-options "-O2 -Warray-bounds -fdiagnostics-show-context=3" } */
/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */
#define MAX_LENGTH 10
int a[MAX_LENGTH];

void __attribute__ ((noinline))  foo (int i, bool is_dollar)
{
  if (i < MAX_LENGTH)
    {
      if (i == -1)
        {
          if (is_dollar)
            __builtin_printf ("dollar");
          else
            __builtin_printf ("euro");
          a[i] = -1; /* { dg-warning "is below array bounds of" } */
        }
      else
        a[i] = i;
    }
  else
    a[i] = i + 1; /* { dg-warning "is above array bounds of" } */
}

int main ()
{
  for (int i = 0; i < MAX_LENGTH; i++)
    foo (i, true);
  return 0;
}

/* { dg-begin-multiline-output "" }
   NN |     a[i] = i + 1;
      |     ~^~~
  'foo': events 1-2
   NN |   if (i < MAX_LENGTH)
      |      ^
      |      |
      |      (1) when the condition is evaluated to false
......
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN |     a[i] = i + 1;
      |     ~~~~
      |      |
      |      (2) warning happens here
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN | int a[MAX_LENGTH];
      |     ^
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN |           a[i] = -1;
      |           ~^~~
  'foo': events 1-3
   NN |   if (i < MAX_LENGTH)
      |      ~    
      |      |
      |      (2) when the condition is evaluated to true
   NN |     {
   NN |       if (i == -1)
      |          ^
      |          |
      |          (1) when the condition is evaluated to true
......
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN |           a[i] = -1;
      |           ~~~~
      |            |
      |            (3) warning happens here
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN | int a[MAX_LENGTH];
      |     ^
   { dg-end-multiline-output "" } */

