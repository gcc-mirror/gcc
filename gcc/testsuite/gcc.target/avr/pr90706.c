/* { dg-do compile } */
/* { dg-options "-Os" } */
 
unsigned char check(float x)
{
   return (0.0 < x);
}
 /* { dg-final { scan-assembler-not "ldd" } } */
 /* { dg-final { scan-assembler-not "std" } } */
