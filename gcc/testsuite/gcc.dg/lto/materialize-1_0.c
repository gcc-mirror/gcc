/* { dg-lto-options {{ -O3 -fwhopr}} } */
/* { dg-lto-do run } */
extern void clone_me (int, int);
int a=15;
inline int inline_me ()
{
  clone_me (0,a);
}
