/* Check the vectorization of existing testsuite examples  */
/* Taken from pr109331.c  */
/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

char *ustol_dpp;
void ustol(int flags)
{
  char *s;
  if (s)
    flags |= 3;
  switch (flags & 3)
  case 3:
    while (*s)
    case '+':
      ++s;
  if (flags)
    ustol_dpp = s;
}

/* { dg-final { scan-tree-dump "Loop being analyzed as uncounted." "vect" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
