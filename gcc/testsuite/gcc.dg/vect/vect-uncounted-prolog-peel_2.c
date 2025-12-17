/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

char b;
int c(char *d) {
  int *a = 0;
  while (*d) {
    while (*a)
      if (*a++ == 1)
        return 1;
    d++;
  }
}
void e() {
  c(&b);
  char *f = &b;
  while (f[0])
    ++b;
}

/* { dg-final { scan-tree-dump {note:\s*Alignment of access forced using peeling.} "vect" } } */
/* { dg-final { scan-tree-dump {if \(ivtmp_[0-9_]+ >= prolog_loop_niters.[0-9_]+\)\n\s*goto} "vect" } } */
/* { dg-final { scan-tree-dump {vectorized 1 loops in function} "vect" } } */
