/* { dg-lto-options {{ -O3 -flto -flto-partition=1to1 -fgnu89-inline }} } */
/* { dg-lto-do run } */
extern void clone_me (int, int);
int a=15;
inline int inline_me ()
{
  clone_me (0,a);
}
