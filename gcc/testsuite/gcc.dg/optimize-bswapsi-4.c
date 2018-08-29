/* { dg-do compile } */
/* { dg-require-effective-target bswap } */
/* { dg-options "-O2 -fdump-tree-bswap" } */
/* { dg-additional-options "-march=z900" { target s390-*-* } } */

typedef unsigned char u8;
typedef unsigned int u32;
union __anonunion
{
  u32 value;
  u8 bytes[4];
};

u32
acpi_ut_dword_byte_swap (u32 value)
{
  union __anonunion in;
  in.value = value;
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  return ((in.bytes[0] << 24) | (in.bytes[1] << 16)
	  | (in.bytes[2] << 8) | in.bytes[3]);
#else
  return ((in.bytes[3] << 24) | (in.bytes[2] << 16)
	  | (in.bytes[1] << 8) | in.bytes[0]);
#endif
}

/* { dg-final { scan-tree-dump "32 bit bswap implementation found at" "bswap" } } */
