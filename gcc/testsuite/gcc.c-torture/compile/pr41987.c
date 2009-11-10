/* PR tree-optimization/41987 */

#define TESTIT(TYPE) do { \
  _Complex TYPE ylm; \
  TYPE nbond; \
  ylm = 0; \
  nbond = 0; \
  ylm = ylm / nbond; \
} while (0)

void qparm_colvar(void)
{
  TESTIT (float);
  TESTIT (double);
  TESTIT (long double);

  TESTIT (char);
  TESTIT (short);
  TESTIT (int);
  TESTIT (long);
  TESTIT (long long);
}
