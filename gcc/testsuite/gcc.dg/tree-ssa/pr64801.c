/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
int a;
int
elantech_detect (void)
{
  return -38;
}
inline int
fsp_detect (void)
{
  return -38;
}
void
psmouse_extensions (void)
{
  int (*b)() = fsp_detect;
  a = b ();
}
/* { dg-final { scan-tree-dump-not "fsp_detect" "optimized" } } */
