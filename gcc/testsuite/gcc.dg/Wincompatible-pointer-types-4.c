/* { dg-do compile } */
/* { dg-options "-std=c90 -pedantic-errors" } */

void *
f1 (int flag)
{
  return flag ? __builtin_memcpy : __builtin_memcmp; /* { dg-warning "pointer type mismatch in conditional expression \\\[-Wincompatible-pointer-types\\\]" } */
  /* { dg-note "first expression has type 'void \\* \\(\\*\\)\\(void \\*," "" { target *-*-* } .-1 } */
  /* { dg-note "second expression has type 'int \\(\\*\\)\\(const void \\*," "" { target *-*-* } .-2 } */
}
