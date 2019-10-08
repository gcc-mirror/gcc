/* PR target/91769 */
/* { dg-do compile } */
/* { dg-skip-if "naming registers makes this a code quality test" { *-*-* } { "-O0" "-g" } { "" } } */
/* { dg-options "-EL -mgp32 -mhard-float" } */

NOCOMPRESSION double
foo (void)
{
  register double* pf __asm__ ("$a1");
  __asm__ __volatile__ ("":"=r"(pf));
  double f = *pf;

  if (f != f)
    f = -f;
  return f;
}

/* { dg-final { scan-assembler-not "lw\t\\\$4,0\\(\\\$5\\)\n\tlw\t\\\$5,4\\(\\\$5\\)\n\tldc1\t\\\$.*,0\\(\\\$5\\)" } } */
/* { dg-final { scan-assembler "lw\t\\\$4,0\\(\\\$5\\)\n\tlw\t\\\$5,4\\(\\\$5\\)\n\tmtc1\t\\\$4,\\\$.*\n\tmthc1\t\\\$5,\\\$.*" } } */
