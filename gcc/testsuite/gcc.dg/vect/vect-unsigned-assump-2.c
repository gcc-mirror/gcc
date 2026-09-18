/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_load_lanes } */

#define uint8_t unsigned char
#define uint32_t unsigned int

int foo (const uint8_t *const cur, uint32_t len, uint32_t len_limit,
         uint32_t pos, uint32_t cur_match)
{
  const uint32_t delta = pos - cur_match;
  const uint8_t *pb = cur - delta;

  /* We vectorize this with struct loads right now.  */
  while ((len += 2) != len_limit)
    if (pb[len] != cur[len])
      break;

  return len;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
/* { dg-final { scan-tree-dump "adding no-wrap assumption" "vect" { target { ! ilp32 } } } } */
