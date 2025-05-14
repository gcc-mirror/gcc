/* PR tree-optimization/35468 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dce" } */

char *const f(void)
{
  char *const line = "/dev/ptyXX";
  line[8] = 1;
  return line;
}
