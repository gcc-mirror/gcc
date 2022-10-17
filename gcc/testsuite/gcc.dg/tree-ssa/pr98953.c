/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-bswap-details" } */

int foo(unsigned char *ptr)
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    return ptr[0] + (ptr[1] << 8);
#else
    return ptr[1] + (ptr[0] << 8);
#endif
}

/* { dg-final { scan-tree-dump "16 bit load in target endianness found" "bswap" } } */

