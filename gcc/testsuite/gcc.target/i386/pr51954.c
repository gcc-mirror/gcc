/* PR target/51954 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "adc" } } */

#ifdef __x86_64__
#define TYPE __int128
#else
#define TYPE long long
#endif

TYPE bar (TYPE x)
{
  return -x;
}
