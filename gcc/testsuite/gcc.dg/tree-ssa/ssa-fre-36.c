/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1-details" } */

extern int opening;
extern int middle_game;
int s;
extern int d[1];
void PreEvaluate(int wtm)
{
  int i, j;
  if (opening) {
      d[0]=1;
  }
  else if (middle_game) {
      d[0]=-1;
  }
  if (4 != opening) {
      return;
  }
  s = 1;
}

/* We should be able to CSE the second load of opening.  */

/* { dg-final { scan-tree-dump "Replaced opening" "fre1" } } */
