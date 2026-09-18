/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_early_break } */

#define uint8_t unsigned char
#define uint32_t unsigned int

int foo (const uint8_t *const cur, uint32_t n)
{
  uint32_t i = 15;

  while (i++ != n)
    if (cur[i - 15] != cur[i])
      break;

  return i;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
/* { dg-final { scan-tree-dump "adding no-wrap assumption" "vect" { target { ! ilp32 } } } } */
