/* PR target/70442 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-msse2" { target ia32 } } */

char a, c;
void
fn1 ()
{
  long long b;
  long m;
  int d;
  switch (d)
    {
    case 5:
      b = a;
    }
  b ^= m;
  c = b >> b;
}
