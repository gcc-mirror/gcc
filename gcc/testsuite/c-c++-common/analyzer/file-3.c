typedef struct _IO_FILE FILE;
extern struct _IO_FILE *stderr;

extern FILE *fopen (const char *__restrict __filename,
		    const char *__restrict __modes);
extern int _IO_getc (FILE *stream);

void
test_1 (const char *path)
{
  FILE *f = fopen (path, "r"); /* { dg-message "opened here" } */

  /* Implementation of getc in glibc < 2.28.
     Verify that we know that this doesn't close the file.  */
  _IO_getc (f);

  return; /* { dg-warning "leak of FILE 'f'" } */ 
}
