/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64" } */

extern char *var1;
extern int var2;

void
func (void)
{
  var2 = var1[1] + var1[0];
}
