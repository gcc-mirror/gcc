/* This testcase ICEd on IA-32 at -O2, because loop was calling convert_modes
   between a MODE_FLOAT and MODE_INT class modes.  */

typedef union
{
  double d;
  long long ll;
} A;

void
foo (A x, A **y, A z)
{
  for (; *y; y++)
    if (x.ll == 262 && (*y)->d == z.d)
      break;
}
