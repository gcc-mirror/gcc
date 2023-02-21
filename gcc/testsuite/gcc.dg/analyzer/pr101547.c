char *
fopen (const char *restrict, const char *restrict);

void
k2_uninit (void)
{
  char *setfiles[1];
  int i; /* { dg-message "region created on stack here" } */

  setfiles[i] = fopen ("", ""); /* { dg-warning "use of uninitialized value 'i'" } */
}

void
k2_leak (int i)
{
  char *setfiles[1];

  setfiles[i] = fopen ("", "");
} /* { dg-warning "leak of FILE" } */
