/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-sink1-details -fdump-tree-cddce2-details" } */

static int b=4;
int c;

int
main()
{
  int e[5] = {1,1,1,1,1};
  for (; b >= 0; b--) {
    c = e[b];
  }
  return 0;
}

/* We should sink e[b] out of the loop which is possible after
   applying store motion to c and b.  */
/* { dg-final { scan-tree-dump "Sinking # VUSE" "sink1" } } */
/* And remove the loop after final value replacement.  */
/* { dg-final { scan-tree-dump "fix_loop_structure: removing loop" "cddce2" } } */
