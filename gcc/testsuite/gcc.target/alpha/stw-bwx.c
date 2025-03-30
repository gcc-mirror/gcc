/* { dg-do compile } */
/* { dg-options "-mbwx" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

void
stw (short *p, short v)
{
  *p = v;
}

/* Expect assembly such as:

	stw $17,0($16)
 */

/* { dg-final { scan-assembler-times "\\sstw\\s\\\$17,0\\\(\\\$16\\\)\\s" 1 } } */
