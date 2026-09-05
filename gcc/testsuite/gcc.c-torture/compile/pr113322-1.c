/* { dg-additional-options "-march=sapphirerapids" { target x86 } } */
/* { dg-require-effective-target avx512f { target x86 } } */
/* PR middle-end/113322 */

float a[16];
void
foo ()
{
int i;
for (i = 0; i < 16/2; i++)
 {
 if (a[2*i+((0 +1)%2)] != (3 * (2*i+((0 +1)%2)) + 2))
  __builtin_abort ();
 }
}
