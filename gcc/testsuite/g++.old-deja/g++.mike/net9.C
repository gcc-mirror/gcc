// { dg-do assemble  }
// { dg-options "-pedantic-errors" }

const int ci=10, *pc = &ci, *const cpc = pc, **ppc;
int i, *p, *const cp = &i;

int main()
{
  ci = 1;	// { dg-error "" } bad
  ci++;		// { dg-error "" } bad
  *pc = 2;	// { dg-error "" } bad
  cp = &ci;	// { dg-error "" } bad
  cpc++;	// { dg-error "" } bad
  p = pc;	// { dg-error "" } bad
}
