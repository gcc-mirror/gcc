/* { dg-do compile } */
/* { dg-options "-O0 -minline-all-stringops -minline-stringops-dynamically -march=core2" } */

void bar (float *);

void foo (void)
{
  float b[256] = {0};
  bar(b);
}
