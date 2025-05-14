/* { dg-do compile } */
/* { dg-options "-mbwx" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

void
stb (char *p, char v)
{
  *p = v;
}

/* Expect assembly such as:

	stb $17,0($16)
 */

/* { dg-final { scan-assembler-times "\\sstb\\s\\\$17,0\\\(\\\$16\\\)\\s" 1 } } */
