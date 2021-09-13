/* Test support of scalar_storage_order attribute */

/* { dg-do compile } */

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define REV_ENDIANNESS __attribute__((scalar_storage_order("big-endian")))
#else
#define REV_ENDIANNESS __attribute__((scalar_storage_order("little-endian")))
#endif

typedef struct tIp6Addr
{
    unsigned int s6_addr32[4];
} tIp6Addr;

struct _tBeTimNetAddr
{
    unsigned char isIPv4;
    union
    {
        unsigned int addr;
        tIp6Addr addr6;   /* { dg-warning "type punning toggles" } */
    } REV_ENDIANNESS u;
} REV_ENDIANNESS;
