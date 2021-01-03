/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void kill (void);

void foo (unsigned int arg)
{
  int a = arg - 3;
  unsigned int b = 4;
  int x = 0x1 << arg;

  if (a < 0)
    b = x;

  /* In the fullness of time, we will delete this call.  */
  if (b >=  5)
    kill ();;
}
