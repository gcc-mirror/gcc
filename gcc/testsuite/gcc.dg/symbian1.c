/* { dg-do compile { target arm*-*-symbianelf* } } */
/* Symbian OS requires that all defined symbols with external linkage
   have the ELF STV_HIDDEN attribute set by default.  */
/* { dg-final { scan-assembler ".hidden.*i" } } */
/* { dg-final { scan-assembler ".hidden.*j" } } */
/* { dg-final { scan-assembler ".hidden.*f" } } */

int i;
int j = 3;
void f() {}

