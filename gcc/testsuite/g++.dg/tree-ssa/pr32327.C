// { dg-do run { target { { i?86-*-* x86_64-*-* } && ilp32 } } }
// { dg-options "-O2" }

typedef unsigned long long uint64;
typedef __SIZE_TYPE__ size_t;

extern "C" {
extern void *memcpy (void *__restrict __dest,
      __const void *__restrict __src, size_t __n) /*throw ()*/;
extern void abort (void);
}

extern void foo (void* p);

inline uint64
ghtonll(uint64 x)
{
 // __r is allocated the same stack slot as dest below
 union { unsigned long long int __ll;
         unsigned long int __l[2]; } __w, __r;
 __w.__ll = x;
 __r.__l[0] = (
   {
     register unsigned int __v;
     __asm__ __volatile__ ("bswap %0" : "=r" (__v) :
                           "0" ((unsigned int) (__w.__l[1])));
     __v; });

 __r.__l[1] = (
   {
     register unsigned int __v;
     __asm__ __volatile__ ("bswap %0" : "=r" (__v) :
                           "0" ((unsigned int) (__w.__l[0])));
     __v; });

 return __r.__ll;
}

inline uint64
double_2_uint64 (const double *source)
{
 uint64 dest;  // allocated the same stack slot as __r above
 memcpy(&dest, source, sizeof(dest));
 return dest;
}

inline void
KeyFromUint64(uint64 fp) {
 uint64 norder;
 norder = ghtonll (fp);
 foo((char*)(&norder));
}

void
KeyFromDouble(double x) {
 uint64 n = double_2_uint64 (&x);
 if (n >= 42) {
   n += 1;
 }

 KeyFromUint64(n);
}

#define NUM		0x0123456789abcdefll
#define EXPECTED	0xe0bc9a7856347243ll

void foo (void *x)
{
  if (*((uint64 *)x) != (uint64) EXPECTED)
    abort ();
}

int main ()
{
  if (sizeof (double) != sizeof (uint64))
    return 0;

  if (sizeof (uint64) == sizeof (unsigned long int))
    return 0;

  KeyFromDouble ((double)NUM);

  return 0;
}
