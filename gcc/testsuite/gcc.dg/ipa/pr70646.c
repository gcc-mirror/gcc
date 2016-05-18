/* { dg-do run } */
/* { dg-options "-O2" } */

#pragma GCC optimize("no-unit-at-a-time")

typedef unsigned char u8;
typedef unsigned long long u64;

static inline __attribute__((always_inline)) u64 __swab64p(const u64 *p)
{
 return (__builtin_constant_p((u64)(*p)) ? ((u64)( (((u64)(*p) & (u64)0x00000000000000ffULL) << 56) | (((u64)(*p) & (u64)0x000000000000ff00ULL) << 40) | (((u64)(*p) & (u64)0x0000000000ff0000ULL) << 24) | (((u64)(*p) & (u64)0x00000000ff000000ULL) << 8) | (((u64)(*p) & (u64)0x000000ff00000000ULL) >> 8) | (((u64)(*p) & (u64)0x0000ff0000000000ULL) >> 24) | (((u64)(*p) & (u64)0x00ff000000000000ULL) >> 40) | (((u64)(*p) & (u64)0xff00000000000000ULL) >> 56))) : __builtin_bswap64(*p));
}

static inline u64 wwn_to_u64(void *wwn)
{
 return __swab64p(wwn);
}

void __attribute__((noinline,noclone)) broken(u64* shost)
{
 u8 node_name[8] = { 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF};
 *shost = wwn_to_u64(node_name);
}

void __attribute__((noinline,noclone)) dummy(void)
{
 __builtin_abort();
}

int main(int argc, char* argv[])
{
 u64 v;

 broken(&v);

 if(v != (u64)-1)
  __builtin_abort();

 return 0;
}
