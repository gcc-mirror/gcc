/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-expand" } */

void
f1 (char *p)
{
  __builtin_memcpy (p, "123", 3);
}

/* { dg-final { scan-rtl-dump-times "mem/s/u" 3 "expand" { target mips*-*-* } } } */
/* { dg-final { cleanup-rtl-dump "expand" } } */
