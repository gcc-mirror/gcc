/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp -fno-tree-fre -fdisable-tree-ethread" } */

void kill(void);

void foo (int x, int y, int z)
{
  // Establish y = [-INF, 54]
  if (y < 55)
    return;

  // Establish z == x
  if (z != x)
    return;

  // EVRP should transform this to if (0 != 0)
  if (y < 30)
    x = 0;

  // # x_1 = PHI <x_5(D)(6), 0(7)>
  // The earlier transformation should make the edge from bb7
  // unexecutable, allowing x_1 == x_5 to be registered, and
  // then fold away this condition as well.
  if (x != z)
    kill();

}
/* { dg-final { scan-tree-dump-not "kill" "evrp" } } */
