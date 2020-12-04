/* PR middle-end/94527 - Add an attribute that marks a function as freeing
   an object
   Verify that attribute malloc with one or two arguments has the expected
   effect on diagnostics.
   { dg-options "-Wall -ftrack-macro-expansion=0" } */

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
    FILE *q = fdopen (0);     // { dg-message "returned from a call to 'fdopen'" "note" }
    sink (q);
    release (q);              // { dg-warning "'release' called on pointer returned from a mismatched allocation function" }
  }
  {
    FILE *q = fdopen (0);     // { dg-message "returned from a call to 'fdopen'" "note" }
    sink (q);
    free (q);                 // { dg-warning "'free' called on pointer returned from a mismatched allocation function" }
  }

  {
    FILE *q = fdopen (0);     // { dg-message "returned from a call to 'fdopen'" "note" }
    sink (q);
    q = realloc (q, 7);       // { dg-warning "'realloc' called on pointer returned from a mismatched allocation function" }
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
    sink (q);
    release (q);              // { dg-warning "'release' called on pointer returned from a mismatched allocation function" }
  }
  {
    FILE *q = fdopen (0);
    sink (q);
    free (q);                 // { dg-warning "'free' called on pointer returned from a mismatched allocation function" }
  }

  {
    FILE *q = fdopen (0);
    sink (q);
    q = realloc (q, 7);       // { dg-warning "'realloc' called on pointer returned from a mismatched allocation function" }
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
    p = popen ("2", "r");     // { dg-message "returned from a call to 'popen'" "note" }
    sink (p);
    fclose (p);               // { dg-warning "'fclose' called on pointer returned from a mismatched allocation function" }
  }

  {
    /* freopen() can close a stream open by popen() but pclose() can't
       close the stream returned from freopen().  */
    FILE *p = popen ("2", "r");
    sink (p);
    p = freopen ("3", "r", p);  // { dg-message "returned from a call to 'freopen'" "note" }
    sink (p);
    pclose (p);               // { dg-warning "'pclose' called on pointer returned from a mismatched allocation function" }
  }
}


void test_tmpfile (void)
{
  {
    FILE *p = tmpfile ();
    sink (p);
    fclose (p);
  }

  {
    FILE *p = tmpfile ();
    sink (p);
    p = freopen ("1", "r", p);
    sink (p);
    fclose (p);
  }

  {
    FILE *p = tmpfile ();     // { dg-message "returned from a call to 'tmpfile'" "note" }
    sink (p);
    pclose (p);               // { dg-warning "'pclose' called on pointer returned from a mismatched allocation function" }
  }
}


void warn_malloc (void)
{
  {
    FILE *p = malloc (100);   // { dg-message "returned from a call to 'malloc'" "note" }
    sink (p);
    fclose (p);               // { dg-warning "'fclose' called on pointer returned from a mismatched allocation function" }
  }

  {
    FILE *p = malloc (100);   // { dg-message "returned from a call to 'malloc'" "note" }
    sink (p);
    p = freopen ("1", "r", p);// { dg-warning "'freopen' called on pointer returned from a mismatched allocation function" }
  }

  {
    FILE *p = malloc (100);   // { dg-message "returned from a call to 'malloc'" "note" }
    sink (p);
    pclose (p);               // { dg-warning "'pclose' called on pointer returned from a mismatched allocation function" }
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
    sink (p);
    release (p);
  }

  {
    FILE *p = acquire ();     // { dg-message "returned from a call to 'acquire'" "note" }
    sink (p);
    fclose (p);               // { dg-warning "'fclose' called on pointer returned from a mismatched allocation function" }
  }

  {
    FILE *p = acquire ();     // { dg-message "returned from a call to 'acquire'" "note" }
    sink (p);
    pclose (p);               // { dg-warning "'pclose' called on pointer returned from a mismatched allocation function" }
  }

  {
    FILE *p = acquire ();     // { dg-message "returned from a call to 'acquire'" "note" }
    sink (p);
    p = freopen ("1", "r", p);  // { dg-warning "'freopen' called on pointer returned from a mismatched allocation function" }
    sink (p);
  }

  {
    FILE *p = acquire ();     // { dg-message "returned from a call to 'acquire'" "note" }
    sink (p);
    free (p);               // { dg-warning "'free' called on pointer returned from a mismatched allocation function" }
  }

  {
    FILE *p = acquire ();     // { dg-message "returned from a call to 'acquire'" "note" }
    sink (p);
    p = realloc (p, 123);     // { dg-warning "'realloc' called on pointer returned from a mismatched allocation function" }
    sink (p);
  }
}
