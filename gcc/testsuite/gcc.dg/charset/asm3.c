/* Test for complex asm statements. Make sure it compiles
   then test for some of the asm statements not being translated.  */
/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && ilp32 } } }
   { dg-require-iconv "IBM1047" }
   { dg-final { scan-assembler "std" } }
   { dg-final { scan-assembler "cld" } }
   { dg-final { scan-assembler "rep" } }
   { dg-final { scan-assembler "movsb" } } */
#define size_t int
void *
memmove (void *__dest, __const void *__src, size_t __n)
{
  register unsigned long int __d0, __d1, __d2;
  if (__dest < __src)
    __asm__ __volatile__
      ("cld\n\t"
       "rep\n\t"
       "movsb"
       : "=&c" (__d0), "=&S" (__d1), "=&D" (__d2)
       : "0" (__n), "1" (__src), "2" (__dest)
       : "memory");
  else
    __asm__ __volatile__
      ("std\n\t"
       "rep\n\t"
       "movsb\n\t"
       "cld"
       : "=&c" (__d0), "=&S" (__d1), "=&D" (__d2)
       : "0" (__n), "1" (__n - 1 + (const char *) __src),
         "2" (__n - 1 + (char *) __dest)
       : "memory");
  return __dest;
}
