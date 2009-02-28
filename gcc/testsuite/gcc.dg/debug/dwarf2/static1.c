/* { dg-do compile } */
/* { dg-options "-O2 -gdwarf-2" } */
void
main(void)
{
 static int unused_local_var;
}
/* { dg-final { scan-assembler "unused_local_var" } } */
