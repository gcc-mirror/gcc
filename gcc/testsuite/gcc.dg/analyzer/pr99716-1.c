typedef struct FILE   FILE;

FILE* fopen (const char*, const char*);
int   fclose (FILE*);
int fprintf (FILE *, const char *, ...);

#define NULL ((void *)0)

void
test_1 (void)
{
  int i;

  for (i = 0; i < 2; ++i) {
    FILE *fp = fopen ("/tmp/test", "w");
    fprintf (fp, "hello:%s ", "world");
    fclose (fp); /* { dg-bogus "double 'fclose'" } */
  }
}

void
test_2 (void)
{
  int i;

  for (i = 0; i < 2; ++i) {
    FILE *fp = fopen ("/tmp/test", "w");
    fprintf (fp, "hello");
  }
} /* { dg-warning "leak of FILE 'fp'" } */

FILE *fp3;

void
test_3 (FILE **fpp)
{
  int i;

  for (i = 0; i < 2; ++i) {
    *fpp = fopen ("/tmp/test", "w");
    fprintf (*fpp, "hello");
    fclose (*fpp); /* { dg-bogus "double 'fclose'" } */
    *fpp = NULL;
  }
}
