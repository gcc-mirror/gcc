/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

typedef unsigned char uint8_t;
typedef uint8_t footype __attribute__((vector_size(4)));

void test(uint8_t *ptr, uint8_t *mask)
{
  footype mv;
  ptr = __builtin_assume_aligned (ptr, __alignof__ (footype));
  mask = __builtin_assume_aligned (mask, __alignof__ (footype));
  __builtin_memcpy(&mv, mask, sizeof(mv));
  for (unsigned i = 0; i < 16; i += 4)
    {
      footype temp;
      __builtin_memcpy(&temp, &ptr[i], sizeof(temp));
      temp ^= mv;
      __builtin_memcpy(&ptr[i], &temp, sizeof(temp));
    }
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" { xfail { { s390*-*-* sparc*-*-* } || vect32 } } } } */
