/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-avx -mfpmath=sse" } */

#pragma pack(2)
struct {
  long long int n;
} a;
extern void fn2 (float);
void
fn1 ()
{
  fn2 (a.n);
}
