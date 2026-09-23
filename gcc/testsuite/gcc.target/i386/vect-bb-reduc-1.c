/* { dg-do compile } */
/* { dg-options "-O2" } */

extern unsigned char *var1;
extern unsigned short var2;
void
func (void)
{
  var2 = var1[1] + var1[0];
}

/* We do not want to vectorize the reduction.  */
/* { dg-final { scan-assembler-not "xmm" } } */
