// This used to fail on s390 due to cse removing an insn with a
// REG_EH_REGION without deleting the EH edge.
// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions" }

void
run (int m, double d)
{
  int stack[m];
  int *sp = stack;

  if (d == 1.0)
    *(sp++) = (0);
  else if (d < 1.0)
    *(sp++) = (-1);
}
