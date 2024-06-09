/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-vect-all" } */

int d(unsigned);

void a() {
  char b[8];
  unsigned c = 0;
  while (c < 7 && b[c])
    ++c;
  if (d(c))
    return;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_partial_vectors } } } */
