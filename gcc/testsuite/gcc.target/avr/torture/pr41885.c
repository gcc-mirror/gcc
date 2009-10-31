/* { dg-options "-w -std=c99" } */
/* { dg-do run } */

#include <limits.h>
#include <stdint.h>
#include <stdlib.h>


uint16_t rotl_16a (uint16_t x)
{
    return (x << 8) | (x >> 8);
}
uint16_t rotl_16b (short dummy, uint16_t x)
{
    return (x << 8) | (x >> 8);
}

uint32_t rotl_32a (uint32_t x)
{
    return (x << 8) | (x >> 24);
}
uint32_t rotl_32b (short dummy, uint32_t x)
{
    return (x << 8) | (x >> 24);
}
uint32_t rotl_32c (short dummy, uint32_t x)
{
    return (x << 16) | (x >> 16);
}
uint32_t rotl_32d (short dummy, uint32_t x)
{
    return (x << 24) | (x >> 8);
}
uint32_t rotl_32e (long dummy, uint32_t x)
{
    return (x << 24) | (x >> 8);
}

uint64_t rotl_64 (uint64_t x)
{
    return (x << 56) | (x >> 8);
}

uint64_t rotl_64a (short dummy, uint64_t x)
{
    return (x << 56) | (x >> 8);
}
uint64_t rotl_64b (short dummy, uint64_t x)
{
    return (x << 48) | (x >> 16);
}
uint64_t rotl_64c (short dummy, uint64_t x)
{
    return (x << 40) | (x >> 24);
}
uint64_t rotl_64d (short dummy, uint64_t x)
{
    return (x << 32) | (x >> 32);
}
uint64_t rotl_64e (short dummy, uint64_t x)
{
    return (x << 24) | (x >> 40);
}
uint64_t rotl_64f (short dummy, uint64_t x)
{
    return (x << 16) | (x >> 48);
}
uint64_t rotl_64g (short dummy, uint64_t x)
{
    return (x << 8) | (x >> 56);
}
uint64_t rotl_64h (long dummy, uint64_t x)
{
    return (x << 16) | (x >> 48);
}
 



int main (void)
{
  if (rotl_16a(0x1234) != 0x3412)
    abort();
  if (rotl_16b(0xAA55,0x1234) != 0x3412)
    abort();

uint32_t num32 = 0x12345678;  

  if (rotl_32a(num32) != 0x34567812)
    abort();
  if (rotl_32b(0xAA55,num32) != 0x34567812)
    abort();
  if (rotl_32c(0xAA55,num32) != 0x56781234)
    abort();
  if (rotl_32d(0xAA55,num32) != 0x78123456)
    abort();
  if (rotl_32e(0x1122AA55,num32) != 0x78123456)
    abort();

uint64_t num = 0x123456789ABCDEF0ULL;

 if (rotl_64(num) != 0xF0123456789ABCDEULL)
    abort();
 if (rotl_64a(0xAA55,num) != 0xF0123456789ABCDEULL)
    abort();
 if (rotl_64b(0xAA55,num) != 0xDEF0123456789ABCULL)
    abort();
 if (rotl_64c(0xAA55,num) != 0xBCDEF0123456789AULL)
    abort();
 if (rotl_64d(0xAA55,num) != 0x9ABCDEF012345678ULL)
    abort();
 if (rotl_64e(0xAA55,num) != 0x789ABCDEF0123456ULL)
    abort();
 if (rotl_64f(0xAA55,num) != 0x56789ABCDEF01234ULL)
    abort();
 if (rotl_64g(0xAA55,num) != 0x3456789ABCDEF012ULL)
    abort();
 if (rotl_64h(0x1122AA55,num) != 0x56789ABCDEF01234ULL)
    abort();

  exit (0);
}

