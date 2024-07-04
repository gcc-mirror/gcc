/* { dg-do assemble { target { apxf && { ! ia32 } } } } */
/* { dg-require-effective-target tls } */
/* { dg-options "-mapxf -O2" } */

#define DECL(TYPE) \
extern __thread TYPE TYPE##_a;

DECL(__int128_t)
DECL(__uint128_t)

#define FOO(TYPE, name, op, val) \
TYPE \
thread_func##TYPE##name (void) \
{ \
  return TYPE##_a op val; \
}

FOO(__int128_t, add, +, 0x2000)
FOO(__uint128_t, add, +, 0x2000)

FOO(__int128_t, sub, -, 0x2000)
FOO(__uint128_t, sub, -, 0x2000)

FOO(__int128_t, or, |, 0x2000)
FOO(__uint128_t, or, |, 0x2000)

FOO(__int128_t, and, &, 0x2000)
FOO(__uint128_t, and, &, 0x2000)

FOO(__int128_t, xor, ^, 0x2000)
FOO(__uint128_t, xor, ^, 0x2000)
