/* PR target/68872 */
/* { dg-do assemble { target { powerpc64le-*-* } } } */
/* { dg-options "-mcpu=powerpc64le" } */

/* Verify that -mcpu=powerpc64le passes -mpower8 to the assembler.  */

long
bar (unsigned char *ptr, unsigned char val)
{
  long ret;
  asm volatile ("stbcx. %0,0,%1" :: "r" (val), "r" (ptr));
  asm volatile ("mfcr %0,8" : "=r" (ret) ::);
  return ret;
}
