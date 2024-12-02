/* We don't find this case because in "crc32_generic_shift" function
   loop's iteration number isn't a constant (i < 8 * (int)(len & 3)).  */

#include <stddef.h>
typedef unsigned int u32;
#define __attribute_const__	__attribute__((__const__))
static u32 __attribute_const__ gf2_multiply (u32 x, u32 y, u32 modulus)
{
  u32 product = x & 1 ? y : 0;
  int i;

  for (i = 0; i < 31; i++)
    {
      product = (product >> 1) ^ (product & 1 ? modulus : 0);
      x >>= 1;
      product ^= x & 1 ? y : 0;
    }
  return product;
}

u32 __attribute_const__ crc32_generic_shift(u32 crc, size_t len,
                                                  u32 polynomial)
{
       u32 power = polynomial; /* CRC of x^32 */
       int i;

       /* Shift up to 32 bits in the simple linear way */
       for (i = 0; i < 8 * (int)(len & 3); i++)
               crc = (crc >> 1) ^ (crc & 1 ? polynomial : 0);

       len >>= 2;
       if (!len)
               return crc;

       for (;;) {
               /* "power" is x^(2^i), modulo the polynomial */
               if (len & 1)
                       crc = gf2_multiply(crc, power, polynomial);

               len >>= 1;
               if (!len)
                       break;

               /* Square power, advancing to x^(2^(i+1)) */
               power = gf2_multiply(power, power, polynomial);
       }

       return crc;
}
