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

/* Declare functions with the minimum attributes malloc how they're
   likely going to be declared in <stdio.h>.  */
               int   fclose (FILE*);
A (fclose)     FILE* fdopen (int);
A (fclose)     FILE* fopen (const char*, const char*);
A (fclose)     FILE* fmemopen(void *, size_t, const char *);
A (fclose)     FILE* freopen (const char*, const char*, FILE*);
A (freopen, 3) FILE* freopen (const char*, const char*, FILE*);
A (fclose)     FILE* tmpfile (void);

A (fclose)     FILE* open_memstream (char**, size_t*);
A (fclose)     FILE* open_wmemstream (char**, size_t*);

               int   pclose (FILE*);
A (pclose)     FILE* popen (const char*, const char*);

               void  release (void*);
A (release)    FILE* acquire (void);

void sink (FILE*);


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
    FILE *q = fdopen (0);     // { dg-message "returned from 'fdopen'" "note" }
    sink (q);
    release (q);              // { dg-warning "'release' called on pointer returned from a mismatched allocation function" }
  }
  {
    FILE *q = fdopen (0);     // { dg-message "returned from 'fdopen'" "note" }
    sink (q);
    free (q);                 // { dg-warning "'free' called on pointer returned from a mismatched allocation function" }
  }

  {
    FILE *q = fdopen (0);     // { dg-message "returned from 'fdopen'" "note" }
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


void test_freopen (FILE *p[])
{
  {
    FILE *q = freopen ("1", "r", p[0]);
    sink (q);
    fclose (q);
  }
  {
    FILE *q = freopen ("2", "r", p[1]);
    sink (q);
    q = freopen ("3", "r", q);
    sink (q);
    fclose (q);
  }

  {
    FILE *q;
    q = freopen ("3", "r", p[2]); // { dg-message "returned from 'freopen'" }
    sink (q);
    q = realloc (q, 7);       // { dg-warning "'realloc' called on pointer returned from a mismatched allocation function" }
    sink (q);
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
    FILE *p = tmpfile ();     // { dg-message "returned from 'tmpfile'" "note" }
    sink (p);
    pclose (p);               // { dg-warning "'pclose' called on pointer returned from a mismatched allocation function" }
  }
}


void test_open_memstream (char **bufp, size_t *sizep)
{
  {
    FILE *p = open_memstream (bufp, sizep);
    sink (p);
    fclose (p);
  }

  {
    FILE *p = open_memstream (bufp, sizep);
    sink (p);
    p = freopen ("1", "r", p);
    sink (p);
    fclose (p);
  }

  {
    FILE *p;
    p = open_memstream (bufp, sizep);   // { dg-message "returned from 'open_memstream'" "note" }
    sink (p);
    pclose (p);               // { dg-warning "'pclose' called on pointer returned from a mismatched allocation function" }
  }

  {
    FILE *p;
    p = open_memstream (bufp, sizep);   // { dg-message "returned from 'open_memstream'" "note" }
    sink (p);
    free (p);                 // { dg-warning "'free' called on pointer returned from a mismatched allocation function" }
  }

  {
    FILE *p;
    p = open_memstream (bufp, sizep);   // { dg-message "returned from 'open_memstream'" "note" }
    sink (p);
    release (p);              // { dg-warning "'release' called on pointer returned from a mismatched allocation function" }
  }
}


void test_open_wmemstream (char **bufp, size_t *sizep)
{
  {
    FILE *p = open_wmemstream (bufp, sizep);
    sink (p);
    fclose (p);
  }

  {
    FILE *p = open_wmemstream (bufp, sizep);
    sink (p);
    p = freopen ("1", "r", p);
    sink (p);
    fclose (p);
  }

  {
    FILE *p;
    p = open_wmemstream (bufp, sizep);  // { dg-message "returned from 'open_wmemstream'" "note" }
    sink (p);
    pclose (p);               // { dg-warning "'pclose' called on pointer returned from a mismatched allocation function" }
  }

  {
    FILE *p;
    p = open_wmemstream (bufp, sizep);  // { dg-message "returned from 'open_wmemstream'" "note" }
    sink (p);
    free (p);                 // { dg-warning "'free' called on pointer returned from a mismatched allocation function" }
  }

  {
    FILE *p;
    p = open_wmemstream (bufp, sizep);  // { dg-message "returned from 'open_wmemstream'" "note" }
    sink (p);
    release (p);              // { dg-warning "'release' called on pointer returned from a mismatched allocation function" }
  }
}


void warn_malloc (void)
{
  {
    FILE *p = malloc (100);   // { dg-message "returned from 'malloc'" "note" }
    sink (p);
    fclose (p);               // { dg-warning "'fclose' called on pointer returned from a mismatched allocation function" }
  }

  {
    FILE *p = malloc (100);   // { dg-message "returned from 'malloc'" "note" }
    sink (p);
    p = freopen ("1", "r", p);// { dg-warning "'freopen' called on pointer returned from a mismatched allocation function" }
  }

  {
    FILE *p = malloc (100);   // { dg-message "returned from 'malloc'" "note" }
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
    FILE *p = acquire ();     // { dg-message "returned from 'acquire'" "note" }
    sink (p);
    fclose (p);               // { dg-warning "'fclose' called on pointer returned from a mismatched allocation function" }
  }

  {
    FILE *p = acquire ();     // { dg-message "returned from 'acquire'" "note" }
    sink (p);
    pclose (p);               // { dg-warning "'pclose' called on pointer returned from a mismatched allocation function" }
  }

  {
    FILE *p = acquire ();     // { dg-message "returned from 'acquire'" "note" }
    sink (p);
    p = freopen ("1", "r", p);  // { dg-warning "'freopen' called on pointer returned from a mismatched allocation function" }
    sink (p);
  }

  {
    FILE *p = acquire ();     // { dg-message "returned from 'acquire'" "note" }
    sink (p);
    free (p);               // { dg-warning "'free' called on pointer returned from a mismatched allocation function" }
  }

  {
    FILE *p = acquire ();     // { dg-message "returned from 'acquire'" "note" }
    sink (p);
    p = realloc (p, 123);     // { dg-warning "'realloc' called on pointer returned from a mismatched allocation function" }
    sink (p);
  }
}
