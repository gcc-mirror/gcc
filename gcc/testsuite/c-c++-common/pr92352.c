/* PR inline-asm/92352 */

void
foo (int x)
{
  int var[x];
  asm volatile ("" : "+r" (var));	/* { dg-error "impossible constraint in 'asm'" } */
}					/* { dg-error "non-memory output 0 must stay in memory" "" { target *-*-* } .-1 } */

void
bar (int x)
{
  int var[x];
  asm volatile ("" : "+m" (var));
}
