/* PR target/103205 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune-ctrl=^himode_math" } */

unsigned short a;

unsigned short
foo (void)
{
  return __sync_fetch_and_and (&a, ~1) & 1;
}
