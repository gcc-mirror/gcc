/* { dg-do compile } */
/* { dg-options "-O2" } */

void
test (void)
{
  int x, y;

  __asm__ __volatile__ ("" : "=a" (x), "={rbx}" (y));
  __asm__ __volatile__ ("" : "=a" (x), "={rax}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "=a" (x) : "{rax}" (y));
  __asm__ __volatile__ ("" : "=&a" (x) : "{rax}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" :: "a" (x), "{rax}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */

  __asm__ __volatile__ ("" : "={rbx}" (x), "=a" (y));
  __asm__ __volatile__ ("" : "={rax}" (x), "=a" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "={rax}" (x) : "a" (y));
  __asm__ __volatile__ ("" : "=&{rax}" (x) : "a" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" :: "{rax}" (x), "a" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */

  __asm__ __volatile__ ("" : "=b" (x), "={rax}" (y));
  __asm__ __volatile__ ("" : "=b" (x), "={rbx}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "=b" (x) : "{rbx}" (y));
  __asm__ __volatile__ ("" : "=&b" (x) : "{rbx}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" :: "b" (x), "{rbx}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */

  __asm__ __volatile__ ("" : "={rax}" (x), "=b" (y));
  __asm__ __volatile__ ("" : "={rbx}" (x), "=b" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "={rbx}" (x) : "b" (y));
  __asm__ __volatile__ ("" : "=&{rbx}" (x) : "b" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" :: "{rbx}" (x), "b" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */

  __asm__ __volatile__ ("" : "=c" (x), "={rax}" (y));
  __asm__ __volatile__ ("" : "=c" (x), "={rcx}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "=c" (x) : "{rcx}" (y));
  __asm__ __volatile__ ("" : "=&c" (x) : "{rcx}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" :: "c" (x), "{rcx}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */

  __asm__ __volatile__ ("" : "={rax}" (x), "=c" (y));
  __asm__ __volatile__ ("" : "={rcx}" (x), "=c" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "={rcx}" (x) : "c" (y));
  __asm__ __volatile__ ("" : "=&{rcx}" (x) : "c" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" :: "{rcx}" (x), "c" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */

  __asm__ __volatile__ ("" : "=d" (x), "={rax}" (y));
  __asm__ __volatile__ ("" : "=d" (x), "={rdx}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "=d" (x) : "{rdx}" (y));
  __asm__ __volatile__ ("" : "=&d" (x) : "{rdx}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" :: "d" (x), "{rdx}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */

  __asm__ __volatile__ ("" : "={rax}" (x), "=d" (y));
  __asm__ __volatile__ ("" : "={rdx}" (x), "=d" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "={rdx}" (x) : "d" (y));
  __asm__ __volatile__ ("" : "=&{rdx}" (x) : "d" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" :: "{rdx}" (x), "d" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */

  __asm__ __volatile__ ("" : "=S" (x), "={rax}" (y));
  __asm__ __volatile__ ("" : "=S" (x), "={rsi}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "=S" (x) : "{rsi}" (y));
  __asm__ __volatile__ ("" : "=&S" (x) : "{rsi}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" :: "S" (x), "{rsi}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */

  __asm__ __volatile__ ("" : "={rax}" (x), "=S" (y));
  __asm__ __volatile__ ("" : "={rsi}" (x), "=S" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "={rsi}" (x) : "S" (y));
  __asm__ __volatile__ ("" : "=&{rsi}" (x) : "S" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" :: "{rsi}" (x), "S" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */

  __asm__ __volatile__ ("" : "=D" (x), "={rax}" (y));
  __asm__ __volatile__ ("" : "=D" (x), "={rdi}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "=D" (x) : "{rdi}" (y));
  __asm__ __volatile__ ("" : "=&D" (x) : "{rdi}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" :: "D" (x), "{rdi}" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */

  __asm__ __volatile__ ("" : "={rax}" (x), "=D" (y));
  __asm__ __volatile__ ("" : "={rdi}" (x), "=D" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" : "={rdi}" (x) : "D" (y));
  __asm__ __volatile__ ("" : "=&{rdi}" (x) : "D" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
  __asm__ __volatile__ ("" :: "{rdi}" (x), "D" (y)); /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
}
