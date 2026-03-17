/* PR target/124461 */
/* { dg-do compile { target { masm_intel && { ! ia32 } } } } */
/* { dg-options "-O2 -mmovrs -masm=intel" } */
/* { dg-final { scan-assembler "\tmovrs\teax, DWORD PTR \\\[\[er]di\\\]" } } */
/* { dg-final { scan-assembler "\tmovrs\trax, QWORD PTR \\\[\[er]di\\\]" } } */

#include <x86intrin.h>

int
foo (const void *p)
{
  return _movrs_i32 (p);
}

long long
bar (const void *p)
{
  return _movrs_i64 (p);
}
