/* { dg-do link } */
enum A { A1 = (unsigned long)-1 } a;
enum B { B1 = (unsigned long long)-1, B2 = 0x123456789abcdef0ULL } b;
#ifdef __SIZEOF_INT128__
enum C { C1 = (__uint128_t)-1, C2 = ((__uint128_t) 0x123456789abcdef0ULL) << 64 | 0x0fedcba987654321ULL } c;
#endif
int
main ()
{
}
