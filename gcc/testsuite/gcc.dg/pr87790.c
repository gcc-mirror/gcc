/* { dg-do compile } */
/* { dg-require-profiling "-fprofile-generate" } */
/* { dg-options "-Ofast -fprofile-generate" } */
/* { dg-additional-options "-march=znver1" { target { x86_64-*-* i?86-*-* } } } */

int a, b;
void c(int d[][8])
{
  int e, f;
  for (; b; b++) {
    e = d[b][0] % 4 * 21;
    if (e >= 21)
      e--;
    a = d[b][0] - e;
    f = 1;
    for (; f != 8; f++)
      d[b][f] = a;
  }
}
