/* { dg-do run } */
/* { dg-options "-Os -fno-inline-functions" } */

void abort (void);

typedef struct {
  int x[7];
} agg7;

typedef struct {
  int mbr1;
  int mbr2;
} agg2;

int expected = 31415;
agg7 filler;

int GetConst (agg7 filler, agg2 split)
{
  return expected;
}

void VerifyValues (agg7 filler, int last_reg, int first_stack, int second_stack)
{
  if (first_stack != 123 || second_stack != expected)
    abort ();
}

void RunTest (agg2 a)
{
  int result;
	
  result = GetConst (filler, a);
  VerifyValues (filler, 0, a.mbr1, result);
}

int main(void)
{
  agg2 result = {123, 456};
  RunTest (result);
  return 0;
}

