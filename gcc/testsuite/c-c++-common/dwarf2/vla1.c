// PR debug/42800
// { dg-options "-gdwarf-2 -dA" }
// { dg-final { scan-assembler "DW_AT_upper_bound" } }

int
f (int i)
{
  char a[i];

  return a[0];
}
