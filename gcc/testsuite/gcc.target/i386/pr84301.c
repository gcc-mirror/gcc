/* PR target/84301 */
/* { dg-do compile } */
/* { dg-options "-march=bdver1 -O1 -fexpensive-optimizations -fschedule-insns -fselective-scheduling -fno-dce -fno-tree-dce --param max-pending-list-length=0 --param selsched-max-lookahead=2" } */

int lr;
long int xl;

int
v4 (void)
{
  int mp;

  ++xl;
  mp = (lr - xl) > 1;
}
