/* { dg-do compile { target *-*-darwin* } } */
/* { dg-require-weak "" } */

/* { dg-final { scan-assembler-not "coalesced" } } */

extern void foo(void) __attribute__((weak_import));

void foo(void)
{
}
