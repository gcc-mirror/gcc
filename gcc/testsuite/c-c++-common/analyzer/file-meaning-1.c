/* { dg-additional-options "-fanalyzer-verbose-state-changes" } */

typedef struct FILE   FILE;
FILE* fopen (const char*, const char*);
int   fclose (FILE*);

void test_1 (const char *path)
{
  FILE *f = fopen (path, "r"); /* { dg-message "meaning: \\{verb: 'acquire', noun: 'resource'\\}" } */
  if (!f)
    return;

  fclose (f); /* { dg-message "meaning: \\{verb: 'release', noun: 'resource'\\}" } */
  fclose (f); /* { dg-warning "double 'fclose' of FILE 'f'" "warning" } */ 
}
