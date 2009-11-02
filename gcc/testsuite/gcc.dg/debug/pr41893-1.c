/* PR debug/41893 */
/* { dg-do compile } */
/* { dg-options "-combine -fwhole-program -O" } */
/* { dg-additional-sources "pr41893-2.c" } */

struct S { int v; };
struct S s;

void __attribute__((externally_visible))
func1 (void)
{
  struct S *p = &s;
}
