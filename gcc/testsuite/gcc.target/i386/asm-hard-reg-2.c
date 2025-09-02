/* { dg-do compile } */
/* { dg-options "-O2" } */

void
test (void)
{
  int x, y, yy;
#ifdef __x86_64__
  int z __attribute__ ((mode (TI)));
#else
  long long z;
#endif

  __asm__ __volatile__ ("" : "=A" (z), "={rbx}" (y));
  __asm__ __volatile__ ("" : "=A" (z), "={rax}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "=A" (z), "={rdx}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "=A" (z) : "{rax}" (y));
  __asm__ __volatile__ ("" : "=A" (z) : "{rdx}" (y));
  __asm__ __volatile__ ("" : "=&A" (z) : "{rax}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "=&A" (z) : "{rdx}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" :: "A" (z), "{rax}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" :: "A" (z), "{rdx}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */

  __asm__ __volatile__ ("" : "={rbx}" (y), "=A" (z));
  __asm__ __volatile__ ("" : "={rax}" (y), "=A" (z)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "={rdx}" (y), "=A" (z)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "={rax}" (y) : "A" (z));
  __asm__ __volatile__ ("" : "={rdx}" (y) : "A" (z));
  __asm__ __volatile__ ("" : "=&{rax}" (y) : "A" (z)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "=&{rdx}" (y) : "A" (z)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" :: "{rax}" (y), "A" (z)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" :: "{rdx}" (y), "A" (z)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */

  /* Note, we do not error for */
  __asm__ __volatile__ ("" : "=A" (x), "={rax}" (y));
  __asm__ __volatile__ ("" : "=A" (x), "={rdx}" (y));
  /* This is due to how constraint A is implemented.  RA has the freedom to
     choose between rax or rdx for operand 0 since x fits into a single
     register and does not require a register pair.  Of course, we error out if
     rax and rdx are taken by other operands as in the following:  */
  __asm__ __volatile__ ("" : "=A" (x), "={rax}" (y), "={rdx}" (yy)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "=A" (x), "={rdx}" (y), "={rax}" (yy)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
}
