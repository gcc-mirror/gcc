void *
calloc (__SIZE_TYPE__, __SIZE_TYPE__);

void *
realloc (void *, __SIZE_TYPE__);

void
foo (void)
{
  int *ap5 = calloc (4, sizeof *ap5);
  int *ap7 = realloc (ap5, sizeof *ap5);
} /* { dg-warning "leak of 'ap5'" "leak of ap5" } */
/* { dg-warning "leak of 'ap7'" "leak of ap7" { target *-*-* } .-1 } */
