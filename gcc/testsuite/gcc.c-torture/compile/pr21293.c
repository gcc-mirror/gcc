/* Testcase from Dale Johannesen <dalej@gcc.gnu.org>. */
struct {
double x[1];
} g;
void k( double *, double*);
void h(int Tmp8)
{
  int i;
  for(i = 1;i <= Tmp8;i++)
    k(&g.x[ + -1],&g.x[ Tmp8 + -1]);
}

