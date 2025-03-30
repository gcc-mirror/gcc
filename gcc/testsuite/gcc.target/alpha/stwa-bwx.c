/* { dg-do compile } */
/* { dg-options "-mbwx" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

typedef union
  {
    int i;
    short c;
  }
short_a;

void
stwa (short_a *p, short v)
{
  p->c = v;
}

/* Expect assembly such as:

	stw $17,0($16)
 */

/* { dg-final { scan-assembler-times "\\sstw\\s\\\$17,0\\\(\\\$16\\\)\\s" 1 } } */
