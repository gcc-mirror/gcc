char *
fopen (const char *restrict, const char *restrict);

void
err (void);

void
k2 (void)
{
  char *setfiles[1];
  int i;

  setfiles[i] = fopen("", ""); /* { dg-warning "use of uninitialized value 'i'" } */
  if (!setfiles[i]) /* { dg-warning "use of uninitialized value 'i'" } */
    err ();
} /* { dg-warning "leak of FILE" } */
