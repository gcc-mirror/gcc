/* { dg-do run } */
/* { dg-options "-O2" } */
#include <limits.h>

extern void abort(void);

void decCompareOp (int result)
{
  if (result != (int) (INT_MAX + 1U))
    {
      result = -result;
      if (result != (int) (INT_MAX + 2U))
        abort ();
    }
}

int main()
{
  decCompareOp (INT_MAX);
  return 0;
}
