/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-expand" } */

#ifdef __mips
__attribute__((nomips16))
#endif
void
f1 (char *p)
{
  __builtin_memcpy (p, "12345", 5);
}

/* { dg-final { scan-rtl-dump "mem/u.*mem/u" "expand" { target mips*-*-* } } } */
/* { dg-final { cleanup-rtl-dump "expand" } } */
