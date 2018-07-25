/* PR tree-optimization/86066 */
/* Testcase by Zhendong Su <Zhendong Su> */

struct A
{
  int b:2;
  int c:2;
  unsigned d:8;
};

int main ()
{
  struct A t = { 0, 0, 2 };
 L:
  t.d = ~(~(~0 % t.d) % 2);
  if (!t.d)
    goto L;
  return 0;
}
