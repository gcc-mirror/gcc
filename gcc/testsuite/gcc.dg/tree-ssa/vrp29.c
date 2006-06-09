/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort(void);

void decCompareOp (int result)
{
  if (result != (int)0x80000000)
    {
      result = -result;
      if (result != (int)0x80000001)
        abort ();
    }
}

int main()
{
  decCompareOp (0x7fffffff);
  return 0;
}
