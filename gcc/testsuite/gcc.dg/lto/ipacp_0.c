/* { dg-lto-options {{ -O1 -flto -flto-partition=1to1 -fipa-cp -fipa-cp-clone}} } */
/* { dg-lto-do run } */

/* Test that clonning happens and we unify declarations of a from both units.  */
const int a = 5;
extern void clone_me (int *);

int
main(void)
{
  int i;
  for (i=0;i<100;i++)
   clone_me ((int *)&a);
  return 0;
}
