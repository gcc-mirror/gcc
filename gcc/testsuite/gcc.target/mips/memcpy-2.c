/* { dg-do compile } */
/* { dg-options "isa_rev<=5 -fdump-rtl-expand" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-Os" } { "" } } */

__attribute__((nomips16))
void
f1 (char *p)
{
  __builtin_memcpy (p, "12345", 5);
}

/* { dg-final { scan-rtl-dump "mem/u.*mem/u" "expand" } } */
