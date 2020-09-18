/* { dg-do run } */

#include <stdio.h>

/* Test A/L, B/H, C, D, d, e, f, g operand modifiers on 32-bit, 64-bit and,
   where appropriate, 16-bit values.  */

#define MEM16_VAL 0x2345
#define MEM32_VAL 0x89abcdef
#define MEM64_VAL 0xfedcba9876543210

#define CONST16_VAL 0xbcde
#define CONST32_VAL 0x99aabbcc
#define CONST64_VAL 0x8899aabbccddeeff

#define REG32_VAL 0x12345678
#define REG64_VAL 0x123456789abcdef

volatile unsigned long mem16 = MEM16_VAL;
volatile unsigned long mem32 = MEM32_VAL;
volatile unsigned long long mem64 = MEM64_VAL;

unsigned int word0, word1, word2, word3;
unsigned char byte0, byte1, byte2, byte3, byte4, byte5, byte6, byte7;

#define CHECK_BYTES_IN_16BIT_VAL(VAL)		\
  if (byte0 != ((unsigned char)VAL)			\
      || byte1 != ((unsigned char)(VAL >> 8)))	\
    return 1;

#define CHECK_WORDS_IN_32BIT_VAL(VAL)		\
  if (word0 != ((unsigned)VAL)			\
      || word1 != ((unsigned)(VAL >> 16)))	\
    return 1;

#define CHECK_WORDS_IN_64BIT_VAL(VAL)		\
  if (word0 != ((unsigned)VAL)		\
      || word1 != ((unsigned)(VAL >> 16))	\
      || word2 != ((unsigned)(VAL >> 32))	\
      || word3 != ((unsigned)(VAL >> 48)))	\
    return 1;

#define CHECK_BYTES_IN_32BIT_VAL(VAL)		\
  if (byte0 != ((unsigned char)VAL)		\
      || byte1 != ((unsigned char)(VAL >> 8))	\
      || byte2 != ((unsigned char)(VAL >> 16))	\
      || byte3 != ((unsigned char)(VAL >> 24)))	\
    return 1;

#define CHECK_BYTES_IN_64BIT_VAL(VAL)		\
  if (byte0 != ((unsigned char)VAL)		\
      || byte1 != ((unsigned char)(VAL >> 8))	\
      || byte2 != ((unsigned char)(VAL >> 16))	\
      || byte3 != ((unsigned char)(VAL >> 24))	\
      || byte4 != ((unsigned char)(VAL >> 32))	\
      || byte5 != ((unsigned char)(VAL >> 40))	\
      || byte6 != ((unsigned char)(VAL >> 48))	\
      || byte7 != ((unsigned char)(VAL >> 56)))	\
    return 1;

int
main (void)
{
  unsigned long register reg32 = REG32_VAL;
  unsigned long long register reg64 = REG64_VAL;

  /* *** MEMORY OPERAND TESTS *** */
  /* Test byte extraction of a 16-bit value.  */
  __asm__("mov.b %A1, %0\n" : "=m" (byte0) : "m" (mem16));
  __asm__("mov.b %d1, %0\n" : "=m" (byte1) : "m" (mem16));
  CHECK_BYTES_IN_16BIT_VAL (MEM16_VAL);

  /* Test extraction of high and low words from 32-bit value.  */
  __asm__("mov %A1, %0\n" : "=m" (word0) : "m" (mem32));
  __asm__("mov %B1, %0\n" : "=m" (word1) : "m" (mem32));
  CHECK_WORDS_IN_32BIT_VAL (MEM32_VAL);

  /* Test extraction of each word of a 64-bit value.  */
  __asm__("mov %A1, %0\n" : "=m" (word0) : "m" (mem64));
  __asm__("mov %B1, %0\n" : "=m" (word1) : "m" (mem64));
  __asm__("mov %C1, %0\n" : "=m" (word2) : "m" (mem64));
  __asm__("mov %D1, %0\n" : "=m" (word3) : "m" (mem64));
  CHECK_WORDS_IN_64BIT_VAL (MEM64_VAL);

  /* Test extraction of each byte of a 32-bit value.  */
  __asm__("mov.b %A1, %0\n" : "=m" (byte0) : "m" (mem32));
  __asm__("mov.b %d1, %0\n" : "=m" (byte1) : "m" (mem32));
  __asm__("mov.b %B1, %0\n" : "=m" (byte2) : "m" (mem32));
  __asm__("mov.b %e1, %0\n" : "=m" (byte3) : "m" (mem32));
  CHECK_BYTES_IN_32BIT_VAL (MEM32_VAL);

  /* Test extraction of each byte of a 64-bit value.  */
  __asm__("mov.b %A1, %0\n" : "=m" (byte0) : "m" (mem64));
  __asm__("mov.b %d1, %0\n" : "=m" (byte1) : "m" (mem64));
  __asm__("mov.b %B1, %0\n" : "=m" (byte2) : "m" (mem64));
  __asm__("mov.b %e1, %0\n" : "=m" (byte3) : "m" (mem64));
  __asm__("mov.b %C1, %0\n" : "=m" (byte4) : "m" (mem64));
  __asm__("mov.b %f1, %0\n" : "=m" (byte5) : "m" (mem64));
  __asm__("mov.b %D1, %0\n" : "=m" (byte6) : "m" (mem64));
  __asm__("mov.b %g1, %0\n" : "=m" (byte7) : "m" (mem64));
  CHECK_BYTES_IN_64BIT_VAL (MEM64_VAL);

  /* *** IMMEDIATE OPERAND TESTS *** */
  /* Test byte extraction of a 16-bit value.  */
  __asm__("mov.b %A1, %0\n" : "=m" (byte0) : "i" (CONST16_VAL));
  __asm__("mov.b %d1, %0\n" : "=m" (byte1) : "i" (CONST16_VAL));
  CHECK_BYTES_IN_16BIT_VAL (CONST16_VAL);

  /* Test extraction of high and low words from 32-bit value.  */
  __asm__("mov %A1, %0\n" : "=m" (word0) : "i" (CONST32_VAL));
  __asm__("mov %B1, %0\n" : "=m" (word1) : "i" (CONST32_VAL));
  CHECK_WORDS_IN_32BIT_VAL (CONST32_VAL);

  /* Test extraction of each word of a 64-bit value.  */
  __asm__("mov %A1, %0\n" : "=m" (word0) : "i" (CONST64_VAL));
  __asm__("mov %B1, %0\n" : "=m" (word1) : "i" (CONST64_VAL));
  __asm__("mov %C1, %0\n" : "=m" (word2) : "i" (CONST64_VAL));
  __asm__("mov %D1, %0\n" : "=m" (word3) : "i" (CONST64_VAL));
  CHECK_WORDS_IN_64BIT_VAL (CONST64_VAL);

  /* Test extraction of each byte of a 32-bit value.  */
  __asm__("mov.b %A1, %0\n" : "=m" (byte0) : "i" (CONST32_VAL));
  __asm__("mov.b %d1, %0\n" : "=m" (byte1) : "i" (CONST32_VAL));
  __asm__("mov.b %B1, %0\n" : "=m" (byte2) : "i" (CONST32_VAL));
  __asm__("mov.b %e1, %0\n" : "=m" (byte3) : "i" (CONST32_VAL));
  CHECK_BYTES_IN_32BIT_VAL (CONST32_VAL);

  /* Test extraction of each byte of a 64-bit value.  */
  __asm__("mov.b %A1, %0\n" : "=m" (byte0) : "i" (CONST64_VAL));
  __asm__("mov.b %d1, %0\n" : "=m" (byte1) : "i" (CONST64_VAL));
  __asm__("mov.b %B1, %0\n" : "=m" (byte2) : "i" (CONST64_VAL));
  __asm__("mov.b %e1, %0\n" : "=m" (byte3) : "i" (CONST64_VAL));
  __asm__("mov.b %C1, %0\n" : "=m" (byte4) : "i" (CONST64_VAL));
  __asm__("mov.b %f1, %0\n" : "=m" (byte5) : "i" (CONST64_VAL));
  __asm__("mov.b %D1, %0\n" : "=m" (byte6) : "i" (CONST64_VAL));
  __asm__("mov.b %g1, %0\n" : "=m" (byte7) : "i" (CONST64_VAL));
  CHECK_BYTES_IN_64BIT_VAL (CONST64_VAL);

  /* *** REGISTER OPERAND TESTS *** */
  /* No extraction of bytes from a single register.  */

  /* Test extraction of high and low words from 32-bit value.  */
  __asm__("mov %A1, %0\n" : "=m" (word0) : "r" (reg32));
  __asm__("mov %B1, %0\n" : "=m" (word1) : "r" (reg32));
  CHECK_WORDS_IN_32BIT_VAL (REG32_VAL);

  /* Test extraction of each word of a 64-bit value.  */
  __asm__("mov %A1, %0\n" : "=m" (word0) : "r" (reg64));
  __asm__("mov %B1, %0\n" : "=m" (word1) : "r" (reg64));
  __asm__("mov %C1, %0\n" : "=m" (word2) : "r" (reg64));
  __asm__("mov %D1, %0\n" : "=m" (word3) : "r" (reg64));
  CHECK_WORDS_IN_64BIT_VAL (REG64_VAL);

  return 0;
}
