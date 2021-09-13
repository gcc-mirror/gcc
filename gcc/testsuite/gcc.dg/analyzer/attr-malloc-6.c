/* Adapted from gcc.dg/Wmismatched-dealloc.c.  */

#define A(...) __attribute__ ((malloc (__VA_ARGS__)))

typedef struct FILE   FILE;
typedef __SIZE_TYPE__ size_t;

void  free (void*);
void* malloc (size_t);
void* realloc (void*, size_t);

int   fclose (FILE*);
FILE* freopen (const char*, const char*, FILE*);
int   pclose (FILE*);

A (fclose) A (freopen, 3)
  FILE* fdopen (int);
A (fclose) A (freopen, 3)
  FILE* fopen (const char*, const char*);
A (fclose) A (freopen, 3)
  FILE* fmemopen(void *, size_t, const char *);
A (fclose) A (freopen, 3)
  FILE* freopen (const char*, const char*, FILE*);
A (pclose) A (freopen, 3)
  FILE* popen (const char*, const char*);
A (fclose) A (freopen, 3)
  FILE* tmpfile (void);

void sink (FILE*);


            void  release (void*);
A (release) FILE* acquire (void);

void nowarn_fdopen (void)
{
  {
    FILE *q = fdopen (0);
    if (!q)
      return;

    fclose (q);
  }

  {
    FILE *q = fdopen (0);
    if (!q)
      return;

    q = freopen ("1", "r", q);
    fclose (q);
  }

  {
    FILE *q = fdopen (0);
    if (!q)
      return;

    sink (q);
  }
}


void warn_fdopen (void)
{
  {
    FILE *q = fdopen (0);     // { dg-message "allocated here" }
    release (q);              // { dg-warning "'release' called on 'q' returned from a mismatched allocation function" }
  }
  {
    FILE *q = fdopen (0);     // { dg-message "allocated here" }
    free (q);                 // { dg-warning "'free' called on 'q' returned from a mismatched allocation function" }
  }

  {
    FILE *q = fdopen (0);     // { dg-message "allocated here" }
    q = realloc (q, 7);       // { dg-warning "'realloc' called on 'q' returned from a mismatched allocation function" }
    sink (q);
  }
}


void nowarn_fopen (void)
{
  {
    FILE *q = fopen ("1", "r");
    sink (q);
    fclose (q);
  }

  {
    FILE *q = fopen ("2", "r");
    sink (q);
    q = freopen ("3", "r", q);
    sink (q);
    fclose (q);
  }

  {
    FILE *q = fopen ("4", "r");
    sink (q);
  }
}


void warn_fopen (void)
{
  {
    FILE *q = fopen ("1", "r");
    release (q);              // { dg-warning "'release' called on 'q' returned from a mismatched allocation function" }
    fclose (q);
  }
  {
    FILE *q = fdopen (0);
    free (q);                 // { dg-warning "'free' called on 'q' returned from a mismatched allocation function" }
  }

  {
    FILE *q = fdopen (0);
    q = realloc (q, 7);       // { dg-warning "'realloc' called on 'q' returned from a mismatched allocation function" }
    sink (q);
  }
}


void test_popen (void)
{
  {
    FILE *p = popen ("1", "r");
    sink (p);
    pclose (p);
  }

  {
    FILE *p;
    p = popen ("2", "r");     // { dg-message "allocated here" }
    fclose (p);               // { dg-warning "'fclose' called on 'p' returned from a mismatched allocation function" }
  }

  {
    /* freopen() can close a stream open by popen() but pclose() can't
       close the stream returned from freopen().  */
    FILE *p = popen ("2", "r");
    p = freopen ("3", "r", p);  // { dg-message "allocated here" }
    pclose (p);               // { dg-warning "'pclose' called on 'p' returned from a mismatched allocation function" }
  }
}


void test_tmpfile (void)
{
  {
    FILE *p = tmpfile ();
    fclose (p);
  }

  {
    FILE *p = tmpfile ();
    p = freopen ("1", "r", p);
    fclose (p);
  }

  {
    FILE *p = tmpfile ();     // { dg-message "allocated here" }
    pclose (p);               // { dg-warning "'pclose' called on 'p' returned from a mismatched allocation function" }
  }
}


void warn_malloc (void)
{
  {
    FILE *p = malloc (100);   // { dg-message "allocated here" }
    fclose (p);               // { dg-warning "'p' should have been deallocated with 'free' but was deallocated with 'fclose'" }
  }

  {
    FILE *p = malloc (100);   // { dg-message "allocated here" }
    p = freopen ("1", "r", p);// { dg-warning "'p' should have been deallocated with 'free' but was deallocated with 'freopen'" }
    fclose (p);
  }

  {
    FILE *p = malloc (100);   // { dg-message "allocated here" }
    pclose (p);               // { dg-warning "'p' should have been deallocated with 'free' but was deallocated with 'pclose'" }
  }
}


void test_acquire (void)
{
  {
    FILE *p = acquire ();
    release (p);
  }

  {
    FILE *p = acquire ();
    release (p);
  }

  {
    FILE *p = acquire ();     // { dg-message "allocated here \\(expects deallocation with 'release'\\)" }
    fclose (p);               // { dg-warning "'p' should have been deallocated with 'release' but was deallocated with 'fclose'" }
  }

  {
    FILE *p = acquire ();     // { dg-message "allocated here \\(expects deallocation with 'release'\\)" }
    pclose (p);               // { dg-warning "'p' should have been deallocated with 'release' but was deallocated with 'pclose'" }
  }

  {
    FILE *p = acquire ();      // { dg-message "allocated here \\(expects deallocation with 'release'\\)" }
    p = freopen ("1", "r", p); // { dg-warning "'p' should have been deallocated with 'release' but was deallocated with 'freopen'" }
    sink (p);
  }

  {
    FILE *p = acquire ();   // { dg-message "allocated here \\(expects deallocation with 'release'\\)" }
    free (p);               // { dg-warning "'p' should have been deallocated with 'release' but was deallocated with 'free'" }
  }

  {
    FILE *p = acquire ();     // { dg-message "allocated here \\(expects deallocation with 'release'\\)" }
    p = realloc (p, 123);     // { dg-warning "'p' should have been deallocated with 'release' but was deallocated with 'realloc'" }
    sink (p);
  }
}
