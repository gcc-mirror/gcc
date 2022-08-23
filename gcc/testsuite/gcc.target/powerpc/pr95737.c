/* PR target/95737 */
/* { dg-do compile } */
/* Disable isel for P9 and later.  */
/* { dg-options "-O2 -mno-isel" } */
/* { dg-final { scan-assembler-not {\mextsw\M} } } */


unsigned long negativeLessThan (unsigned long a, unsigned long b)
{
   return -(a < b);
}
