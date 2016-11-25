/* { dg-do compile { target fpic } } */
/* { dg-require-effective-target bswap32 } */
/* { dg-options "-O2 -fPIC -fdump-tree-bswap" } */
/* { dg-additional-options "-march=z900" { target s390*-*-* } } */

void mach_parse_compressed(unsigned char* ptr, unsigned long int* val)
{
  if (ptr[0] < 0xC0U) {
    *val = ptr[0] + ptr[1];
    return;
  }

  *val = ((unsigned long int)(ptr[0]) << 24)
    | ((unsigned long int)(ptr[1]) << 16)
    | ((unsigned long int)(ptr[2]) << 8)
    | ptr[3];
}

/* { dg-final { scan-tree-dump-not "load_dst_\\d+ =.* if \\(" "bswap" } } */
