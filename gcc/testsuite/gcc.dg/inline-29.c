/* PR c/36507 */
/* { dg-do run } */
/* { dg-options "-O0 -std=gnu99" } */

int
main (void)
{
  int i = 2;
  auto inline int f1 (void)
  {
    return i;
  }
  inline int f2 (void)
  {
    return i;
  }
  auto inline int f3 (void);
  auto inline int f3 (void)
  {
    return i;
  }
  auto inline int f4 (void);
  inline int f4 (void)
  {
    return i;
  }
  return f1 () + f2 () + f3 () + f4 () - 8;
}
