// Build don't link:
// Special g++ Options: -pedantic-errors

const int ci=10, *pc = &ci, *const cpc = pc, **ppc;
int i, *p, *const cp = &i;

int main()
{
  i = ci;
  *cp = ci;
  pc++;
  pc = cpc;
  pc = p;
  ppc = &pc;
}
