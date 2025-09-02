/* PR tree-optimization/109071 need more context for -Warray-bounds warnings
   due to compiler optimization.  */
/* { dg-options "-O2 -Warray-bounds -fdiagnostics-show-context=1" } */
/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */
#define MAX_LENGTH 10
int a[MAX_LENGTH];

void __attribute__ ((noinline))  foo (int i, bool is_dollar,
				      bool is_hour, bool is_color)
{
  if (i == -1)
    {
      if (is_dollar)
	__builtin_printf ("dollar");
      else
	__builtin_printf ("euro");
      if (is_hour)
	__builtin_printf ("hour");
      else
	{
	  if (is_color)
	    __builtin_printf ("color minute");
	  else		
	    __builtin_printf ("non minute");
	}
      a[i] = -1; /* { dg-warning "is below array bounds of" } */
    }
  else
    a[i] = i;
}

int main ()
{
  for (int i = 0; i < MAX_LENGTH; i++)
    foo (i, true, false, true);
  return 0;
}

/* { dg-begin-multiline-output "" }
   NN |       a[i] = -1;
      |       ~^~~
  'foo': events 1-2
   NN |   if (i == -1)
      |      ^
      |      |
      |      (1) when the condition is evaluated to true
......
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN |       a[i] = -1;
      |       ~~~~
      |        |
      |        (2) warning happens here
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN | int a[MAX_LENGTH];
      |     ^
   { dg-end-multiline-output "" } */
