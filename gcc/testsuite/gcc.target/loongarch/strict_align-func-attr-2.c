/* { dg-do compile } */
/* { dg-options "-O2 -mno-strict-align" } */
extern char a[8];
extern char b[8];

__attribute__ ((target ("strict-align")))
void
test (void)
{
  a[0] = b[1];	
  a[1] = b[2];	
  a[2] = b[3];	
  a[3] = b[4];	
}


/* { dg-final { scan-assembler-not "ld.w" } } */
