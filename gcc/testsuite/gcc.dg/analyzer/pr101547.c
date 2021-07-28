char *
fopen (const char *restrict, const char *restrict);

void
k2 (void)
{
  char *setfiles[1];
  int i;

  setfiles[i] = fopen ("", ""); /* { dg-warning "use of uninitialized value 'i'" } */
} /* { dg-warning "leak of FILE" } */
