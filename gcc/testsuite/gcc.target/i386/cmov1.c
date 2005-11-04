/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -march=k8" } */
/* { dg-final { scan-assembler "sar\[^\\n\]*magic_namea" } } */
/* { dg-final { scan-assembler "sar\[^\\n\]*magic_nameb" } } */
/* { dg-final { scan-assembler "sar\[^\\n\]*magic_namec" } } */
/* { dg-final { scan-assembler "shr\[^\\n\]*magic_named" } } */
/* { dg-final { scan-assembler "shr\[^\\n\]*magic_namee" } } */
/* { dg-final { scan-assembler "shr\[^\\n\]*magic_namef" } } */

/* Check code generation for several conditional moves doable by single arithmetics.  */

int magic_namea;
char magic_nameb;
short magic_namec;
int magic_named;
char magic_namee;
short magic_namef;

unsigned int gen;
m()
{
  magic_namec=magic_namec>=0?0:-1;
  magic_namea=magic_namea>=0?0:-1;
  magic_nameb=magic_nameb>=0?0:-1;
  magic_named=magic_named>=0?0:1;
  magic_namee=magic_namee>=0?0:1;
  magic_namef=magic_namef>=0?0:1;
}

