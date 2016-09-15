/* { dg-do compile } */
/* { dg-require-effective-target vect_condition } */
/* { dg-skip-if "need vect_max_reduc support" { ! vect_max_reduc } } */

extern void d(void);
void a() {
  char *b;
  char c = 0;
  for (; b < (char *)a; b++) {
    if (*b)
      c = 1;
    *b = 0;
  }
  if (c)
    d();
}
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
