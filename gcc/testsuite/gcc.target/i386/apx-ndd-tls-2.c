/* PR target/113711 */
/* { dg-do assemble { target { apxf && { ! ia32 } } } } */
/* { dg-require-effective-target tls } */
/* { dg-options "-mapxf -O2" } */

#include <stdint.h>

#define DECL(TYPE) \
__thread TYPE TYPE##_a = 255; \
TYPE * volatile TYPE##_a_in_other_thread = (TYPE *)12345;

DECL(uint64_t)
DECL(uint32_t)

#define FOO(TYPE, name, op, val) \
void * \
thread_func##TYPE##name (void *arg) \
{ \
  TYPE##_a_in_other_thread = &TYPE##_a; \
  TYPE##_a = TYPE##_a op val; \
  *((TYPE *) arg) = TYPE##_a; \
  return (void *)0; \
}

FOO(uint64_t, add, +, 0x2000)
FOO(uint32_t, add, +, 0x2000)

FOO(uint64_t, sub, -, 0x2000)
FOO(uint32_t, sub, -, 0x2000)

FOO(uint64_t, or, |, 0x2000)
FOO(uint32_t, or, |, 0x2000)

FOO(uint64_t, and, &, 0x2000)
FOO(uint32_t, and, &, 0x2000)

FOO(uint64_t, xor, ^, 0x2000)
FOO(uint32_t, xor, ^, 0x2000)
