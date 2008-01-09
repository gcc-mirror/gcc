/* Testcase by Martin Michlmayr <tbm@cyrius.com> */

typedef struct
{
  int data[1024];
}
Lint;
Lint lint_operate (Lint a, long long ammount)
{
  int index;
  Lint ret;
  for (index = 0; index < 24; index++)
    ret.data[index] =
      a.data[index + ammount / 32 + 1] << a.data[index + ammount / 32];
  return ret;
}
