/* Test for printf formats.  Test that the C90 functions get their default
   attributes in strict C90 mode, but the C99 and gettext functions
   do not.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat" } */

/* This may not be correct in the particular case, but allows the
   prototypes to be declared, and we don't try to link.
*/
typedef struct _FILE FILE;
extern FILE *stdout;

typedef __SIZE_TYPE__ size_t;
typedef __builtin_va_list va_list;

extern int fprintf (FILE *, const char *, ...);
extern int printf (const char *, ...);
extern int sprintf (char *, const char *, ...);
extern int vfprintf (FILE *, const char *, va_list);
extern int vprintf (const char *, va_list);
extern int vsprintf (char *, const char *, va_list);

extern int snprintf (char *, size_t, const char *, ...);
extern int vsnprintf (char *, size_t, const char *, va_list);

extern char *gettext (const char *);
extern char *dgettext (const char *, const char *);
extern char *dcgettext (const char *, const char *, int);

void
foo (int i, char *s, size_t n, va_list v0, va_list v1, va_list v2, va_list v3,
     va_list v4, va_list v5, va_list v6, va_list v7, va_list v8)
{
  fprintf (stdout, "%d", i);
  fprintf (stdout, "%ld", i); /* { dg-warning "format" "fprintf" } */
  printf ("%d", i);
  printf ("%ld", i); /* { dg-warning "format" "printf" } */
  sprintf (s, "%d", i);
  sprintf (s, "%ld", i); /* { dg-warning "format" "sprintf" } */
  vfprintf (stdout, "%d", v0);
  vfprintf (stdout, "%Y", v1); /* { dg-warning "format" "vfprintf" } */
  vprintf ("%d", v2);
  vprintf ("%Y", v3); /* { dg-warning "format" "vprintf" } */
  /* The following used to give a bogus warning.  */
  vprintf ("%*.*d", v8);
  vsprintf (s, "%d", v4);
  vsprintf (s, "%Y", v5); /* { dg-warning "format" "vsprintf" } */
  snprintf (s, n, "%d", i);
  snprintf (s, n, "%ld", i);
  vsnprintf (s, n, "%d", v6);
  vsnprintf (s, n, "%Y", v7);
  printf (gettext ("%d"), i);
  printf (gettext ("%ld"), i);
  printf (dgettext ("", "%d"), i);
  printf (dgettext ("", "%ld"), i);
  printf (dcgettext ("", "%d", 0), i);
  printf (dcgettext ("", "%ld", 0), i);
}
