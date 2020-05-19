/* { dg-do compile } */
/* { dg-options "-O3" } */

typedef int int32_t __attribute__((mode (__SI__)));
typedef unsigned uint32_t __attribute__((mode (__SI__)));

void c(int32_t *d, char *g)
{
  char *a, *b, *e;
  int f;
  for (; f; f -= 8) {
      *d++ = *e++ | (uint32_t)*g++ << 8 | (uint32_t)*b++ << 16 |
	  (uint32_t)*a++ << 24;
      *d++ = *e++ | (uint32_t)*g++ << 8 | (uint32_t)*b++ << 16 |
	  (uint32_t)*a++ << 24;
  }
}
