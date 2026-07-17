/* Each arm of the diamond loads from a selected address and advances a
   pointer by a selected amount.  Once the two arm loads are commoned the
   diamond writes two registers on both arms (the load result and the
   advance), which noce_convert_multiple_sets turns into conditional moves.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-ce1" } */
/* { dg-additional-options "-mtune=olympus" } */

#include <stddef.h>
#include <stdint.h>

extern const int16_t lentab[256];

static inline uint32_t
extract (uint32_t val, size_t type)
{
  const uint64_t masks = 0x0000FFFF00FF0000ull;
  return val & (uint32_t) ((masks >> (type * 16)) & 0xFFFF);
}

ptrdiff_t
f (const uint8_t *ip, size_t tag, const uint8_t *end, ptrdiff_t op)
{
  do
    {
      const uint8_t *old_ip = ip;
      ptrdiff_t lmo = lentab[tag];
      size_t type = tag & 3;
      if (type == 0)
	{
	  size_t n = (tag >> 2) + 1;
	  tag = ip[n];
	  ip += n + 1;
	}
      else
	{
	  tag = ip[type];
	  ip += type + 1;
	}
      uint32_t next = (uint32_t) old_ip[0] | ((uint32_t) old_ip[1] << 8);
      ptrdiff_t extracted = extract (next, type);
      op += lmo - extracted;
    }
  while (ip < end);
  return op;
}

/* { dg-final { scan-rtl-dump "if-conversion succeeded through noce_convert_multiple_sets" "ce1" } } */
/* { dg-final { scan-assembler-times "\tcsinc\t" 2 } } */
/* { dg-final { scan-assembler-times "\tldrb\t" 1 } } */
/* { dg-final { scan-assembler-not {\tb(eq|ne)\t} } } */
/* { dg-final { scan-assembler-not {\tcbn?z\t} } } */
/* { dg-final { scan-assembler-not {\ttbn?z\t} } } */
