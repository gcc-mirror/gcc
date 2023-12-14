typedef struct FILE   FILE;

FILE* fopen (const char*, const char*);
int   fclose (FILE*);
#define SEEK_SET        0
int fseek (FILE *, long int, int);

void
test_1 (const char *path)
{
  FILE *f = fopen (path, "r"); /* { dg-message "opened here" } */
  if (!f)
    return;

  fclose (f); /* { dg-message "\\(4\\) \\.\\.\\.to here" "to here" } */
  /* { dg-message "\\(5\\) first 'fclose' here" "first fclose" { target *-*-* } .-1 } */
  fclose (f); /* { dg-warning "double 'fclose' of FILE 'f' \\\[CWE-1341\\\]" "warning" } */ 
  /* { dg-message "second 'fclose' here; first 'fclose' was at \\(5\\)" "second fclose" { target *-*-* } .-1 } */
}

/* Swallow -Wuse-after-free issued for the same problem
   { dg-prune-output "-Wuse-after-free" } */

void
test_2 (const char *src, const char *dst)
{
  FILE *f_in = fopen (src, "r"); /* { dg-message "\\(1\\) opened here" } */
  if (!f_in)
    return;

  FILE *f_out = fopen (src, "w");
  if (!f_out)
    return; /* { dg-warning "leak of FILE 'f_in'" "warning" } */
  /* { dg-message "\\(7\\) 'f_in' leaks here; was opened at \\(1\\)" "event" { target *-*-* } .-1 } */

  fclose (f_out);
  fclose (f_in);
}

void
test_3 (const char *path)
{
  FILE *f = fopen (path, "r"); /* { dg-message "opened here" } */
  return; /* { dg-warning "leak of FILE 'f'" } */ 
}

void
test_4 (const char *path)
{
  FILE *f = fopen (path, "r"); /* { dg-message "opened here" } */

  /* Ensure we know about common fns that are known to not close the
     file (e.g. "fseek").  */
  fseek (f, 1024, SEEK_SET);

  return; /* { dg-warning "leak of FILE 'f'" } */ 
}

void
test_5 (const char *path)
{
  FILE *f = fopen (path, "r"); /* { dg-message "opened here" } */
  return; /* { dg-warning "leak of FILE 'f'" } */ 
}
