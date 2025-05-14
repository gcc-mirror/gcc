/* { dg-do compile } */
/* { dg-options "-mno-bwx -msafe-bwa" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

typedef union
  {
    int i;
    char c;
  }
char_a;

void
stba (char_a *p, char v)
{
  p->c = v;
}

/* Expect assembly such as:

	and $17,0xff,$17
$L2:
	ldl_l $1,0($16)
	bic $1,255,$1
	bis $17,$1,$1
	stl_c $1,0($16)
	beq $1,$L2

   without any INSBL or MSKBL instructions and without address masking.  */

/* { dg-final { scan-assembler-times "\\sldl_l\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstl_c\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sand\\s\\\$\[0-9\]+,0xff,\\\$\[0-9\]+\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sbic\\s\\\$\[0-9\]+,255,\\\$\[0-9\]+\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\sbic\\s\\\$\[0-9\]+,7,\\\$\[0-9\]+\\s" } } */
/* { dg-final { scan-assembler-not "\\s(?:insbl|mskbl)\\s" } } */
