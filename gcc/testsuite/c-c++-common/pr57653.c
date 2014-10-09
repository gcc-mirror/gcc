/* { dg-do compile } */
/* { dg-options "-imacros ${srcdir}/c-c++-common/pr57653.h" } */

__attribute__((used)) static const char s[] = F;

/* { dg-final { scan-assembler-not "command-line" } } */
