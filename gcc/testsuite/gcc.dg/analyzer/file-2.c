typedef struct FILE   FILE;

FILE* fopen (const char*, const char*);
int   fclose (FILE*);

struct foo
{
  FILE *m_f;
};

void test (const char *path)
{
  struct foo f;
  f.m_f = fopen (path, "r");

  if (!f.m_f)
    return; /* { dg-bogus "leak of FILE" } */

  fclose (f.m_f);
  fclose (f.m_f); /* { dg-warning "double 'fclose' of FILE 'f.m_f'" } */
}

/* Swallow -Wuse-after-free issued for the same problem
   { dg-prune-output "-Wuse-after-free" } */
