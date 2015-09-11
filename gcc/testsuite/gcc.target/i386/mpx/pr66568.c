/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fcheck-pointer-bounds -mmpx -O2 -fPIC" } */

extern void exit (int);
int a, b, c;
void *set_test () {
  if (b)
    a ? exit (0) : exit (1);
  b = c;
}
