/* { dg-additional-options "-fanalyzer-verbosity=3" } */

typedef struct FILE   FILE;

FILE* fopen (const char*, const char*);
int   fclose (FILE*);

extern int foo ();
extern void bar ();

void test (const char *path, int flag)
{
  FILE *fp = fopen (path, "r"); /* { dg-message "opened here" } */

  /* We should report this control flow at -fanalyzer-verbosity=3.  */
  if (foo ()) /* { dg-message "branch" } */
    bar ();
  else
    bar ();

  if (flag) /* { dg-message "when 'flag == 0'" } */
    fclose (fp); 
} /* { dg-warning "leak of FILE 'fp'" } */
