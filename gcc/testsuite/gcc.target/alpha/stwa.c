/* { dg-do compile } */
/* { dg-options "-mno-bwx -mno-safe-bwa" } */
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

	zapnot $17,3,$17
	ldl $1,0($16)
	zapnot $1,252,$1
	bis $17,$1,$17
	stl $17,0($16)

   without any INSWL or MSKWL instructions and without address masking.  */

/* { dg-final { scan-assembler-times "\\sldl\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstl\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\szapnot\\s\\\$\[0-9\]+,3,\\\$\[0-9\]+\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\szapnot\\s\\\$\[0-9\]+,252,\\\$\[0-9\]+\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\sbic\\s\\\$\[0-9\]+,7,\\\$\[0-9\]+\\s" } } */
/* { dg-final { scan-assembler-not "\\s(?:inswl|mskwl)\\s" } } */
