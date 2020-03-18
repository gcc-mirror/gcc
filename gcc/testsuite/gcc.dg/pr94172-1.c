/* PR c/94172 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

extern enum E e;
enum E { l = 0x100000000ULL };

unsigned long long
foo (void)
{
  return e;
}
