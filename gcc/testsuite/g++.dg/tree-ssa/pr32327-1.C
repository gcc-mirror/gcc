// { dg-do run { target { { i?86-*-* x86_64-*-* } && ilp32 } } }
// { dg-options "-O2" }

// Endian sensitive.  This is a little-endian redux.

typedef long long int64;
typedef unsigned long long uint64;
typedef __SIZE_TYPE__ size_t;

extern "C" {
extern void *memcpy (void *__restrict __dest,
      __const void *__restrict __src, size_t __n) /*throw ()*/;
extern void abort (void);
}

inline uint64 Swap64(uint64 ull) {
 uint64 b0 = (ull >>  0) & 0xff;
 uint64 b1 = (ull >>  8) & 0xff;
 uint64 b2 = (ull >> 16) & 0xff;
 uint64 b3 = (ull >> 24) & 0xff;
 uint64 b4 = (ull >> 32) & 0xff;
 uint64 b5 = (ull >> 40) & 0xff;
 uint64 b6 = (ull >> 48) & 0xff;
 uint64 b7 = (ull >> 56) & 0xff;
 return (b0 << 56) | (b1 << 48) | (b2 << 40) | (b3 << 32) |
        (b4 << 24) | (b5 << 16) | (b6 <<  8) | (b7 <<  0);
}

inline void KeyFromUint64(uint64 ull, unsigned char* key) {
 uint64 ull_swap = Swap64(ull);
 memcpy(key, &ull_swap, sizeof(uint64));
}

inline int64 int64_from_double(const double& source) {
 int64 dest;
 memcpy(&dest, &source, sizeof(dest));
 return dest;
}

void KeyFromDouble(double x, unsigned char* key) __attribute__ ((noinline));
void KeyFromDouble(double x, unsigned char* key) {
 int64 n = int64_from_double(x);
 if (n >= 0) {
   n += 1ull << 63;
 } else {
   n = -n;
 }
 KeyFromUint64(n, key);
}


void TestKeyFromDouble(uint64 ull) {
 double d;
 memcpy(&d, &ull, sizeof(d));

 unsigned char key[sizeof(uint64)];
 unsigned char expected_key[sizeof(uint64)] = { 0x81, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef };

 KeyFromDouble(d, key);

 for (size_t i = 0; i < sizeof(key); ++i) {
   if ((key[i] & 0xff) != expected_key[i])
     abort ();
 }
}

int main() {
 TestKeyFromDouble(0x0123456789abcdefull);
 return 0;
}
