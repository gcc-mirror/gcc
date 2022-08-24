/* Verify that we check for uninitialized values passed to functions
   that we have special-cased state-machine handling for.  */

typedef struct FILE   FILE;

FILE* fopen (const char*, const char*);
int   fclose (FILE*);
int fseek (FILE *, long, int);

FILE *
test_fopen_uninit_path (void)
{
  const char *path;
  FILE *f = fopen (path, "r"); /* { dg-warning "use of uninitialized value 'path'" } */
  return f;
}

FILE *
test_fopen_uninit_mode (const char *path)
{
  const char *mode;
  FILE *f = fopen (path, mode); /* { dg-warning "use of uninitialized value 'mode'" } */
  return f;
}

void
test_fclose_uninit (void)
{
  FILE *f;
  fclose (f); /* { dg-warning "use of uninitialized value 'f'" } */
}

int
test_fseek_uninit_stream (void)
{
  FILE *stream;
  return fseek (stream, 0, 0); /* { dg-warning "use of uninitialized value 'stream'" } */
}

int
test_fseek_uninit_offset (FILE *stream, int whence)
{
  long offset;
  return fseek (stream, offset, whence); /* { dg-warning "use of uninitialized value 'offset'" } */
}

int
test_fseek_uninit_whence (FILE *stream, long offset)
{
  int whence;
  return fseek (stream, offset, whence); /* { dg-warning "use of uninitialized value 'whence'" } */
}
