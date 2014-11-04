/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O2 -mcpu=power7" } */
/* { dg-final { scan-assembler-not "stfd" } } */

/* PR 47862: Verify caller-save spill of vectors in FP regs do not use
   legacy FP insns, which spill only half the vector.  */
extern vector double dd[15];
void bar (void);

vector double foo() {
  vector double a,b,c,d,e,f,g,h,i,j,k,l,m,n;

  a=dd[1]; b=dd[2]; c=dd[3]; d=dd[4]; e=dd[5]; f=dd[6]; g=dd[7]; h=dd[8]; i=dd[9];
  j=dd[10]; k=dd[11]; l=dd[12]; m=dd[13]; n=dd[14];
  bar();
  return (a+b+c+d+e+f+g+h+i+j+k+l+m+n);
}

