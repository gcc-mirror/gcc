void *calloc (__SIZE_TYPE__, __SIZE_TYPE__);

int
x7 (void)
{
  int **md = calloc (1, 1);

  return md[0][0]; /* { dg-warning "possibly-NULL" "unchecked deref" } */
  /* { dg-warning "leak of 'md'" "leak" { target *-*-* } .-1 } */
}
