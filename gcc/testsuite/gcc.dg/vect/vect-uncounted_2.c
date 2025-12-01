/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

int
foo (int *a0, int *aN, int *b0, int *bN)
{
  int *a = a0;
  int *b = b0;

  for (;a != aN && b != bN; a++, b++)
    *a += *b;
}

/* { dg-final { scan-tree-dump "Loop being analyzed as uncounted." "vect" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
/* Make sure the values used in peeled epilog loop effectively reset the last vectorized iteration.  */
/* { dg-final { scan-tree-dump {<bb ([0-9]+)>[^\n\r]*:.+# (a_[0-9]+) = PHI <a_[0-9]+\([0-9]+\).+<bb [0-9]+>[^\n\r]*:.+# a_[0-9]+ = PHI <\2\(\1\)>} "vect" } } */
/* { dg-final { scan-tree-dump {<bb ([0-9]+)>[^\n\r]*:.+# (b_[0-9]+) = PHI <b_[0-9]+\([0-9]+\).+<bb [0-9]+>[^\n\r]*:.+# b_[0-9]+ = PHI <\2\(\1\)>} "vect" } } */

