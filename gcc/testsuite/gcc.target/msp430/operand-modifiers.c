volatile unsigned long si = 0x89abcdef;
volatile unsigned long long di = 0xfedcba9876543210;

unsigned int a, b, c, d;

int
main (void)
{
  /* Check that %A and %B extract the low and high words of a 32-bit value,
     respectively.  */
  __asm__("mov %A1, %0\n" : "=m" (a) : "m" (si));
  __asm__("mov %B1, %0\n" : "=m" (b) : "m" (si));
  if (a != ((unsigned)si)
      || b != ((unsigned)(si >> 16)))
    return 1;

  /* Check that %A, %B, %C and %D extract the 1st, 2nd, 3rd and 4th words of a
     64-bit value, respectively.  */
  __asm__("mov %A1, %0\n" : "=m" (a) : "m" (di));
  __asm__("mov %B1, %0\n" : "=m" (b) : "m" (di));
  __asm__("mov %C1, %0\n" : "=m" (c) : "m" (di));
  __asm__("mov %D1, %0\n" : "=m" (d) : "m" (di));
  if (a != ((unsigned)di)
      || b != ((unsigned)(di >> 16))
      || c != ((unsigned)(di >> 32))
      || d != ((unsigned)(di >> 48)))
    return 1;

  return 0;
}
