/* Test for printf formats.  Test that the C99 functions get their default
   attributes in strict C99 mode, but the gettext functions do not.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic -Wformat" } */

/* This may not be correct in the particular case, but allows the
   prototypes to be declared, and we don't try to link.
*/
typedef struct _FILE FILE;
extern FILE *stdout;

typedef __SIZE_TYPE__ size_t;
typedef __builtin_va_list va_list;

extern int fprintf (FILE *restrict, const char *restrict, ...);
extern int printf (const char *restrict, ...);
extern int sprintf (char *restrict, const char *restrict, ...);
extern int vfprintf (FILE *restrict, const char *restrict, va_list);
extern int vprintf (const char *restrict, va_list);
extern int vsprintf (char *restrict, const char *restrict, va_list);
extern int snprintf (char *restrict, size_t, const char *restrict, ...);
extern int vsnprintf (char *restrict, size_t, const char *restrict, va_list);

extern char *gettext (const char *);
extern char *dgettext (const char *, const char *);
extern char *dcgettext (const char *, const char *, int);

void
foo (int i, char *s, size_t n, va_list v0, va_list v1, va_list v2, va_list v3,
     va_list v4, va_list v5, va_list v6, va_list v7)
{
  fprintf (stdout, "%d", i);
  fprintf (stdout, "%ld", i); /* { dg-warning "format" "fprintf" } */
  printf ("%d", i);
  printf ("%ld", i); /* { dg-warning "format" "printf" } */
  sprintf (s, "%d", i);
  sprintf (s, "%ld", i); /* { dg-warning "format" "sprintf" } */
  snprintf (s, n, "%d", i);
  snprintf (s, n, "%ld", i); /* { dg-warning "format" "snprintf" } */
  vfprintf (stdout, "%d", v0);
  vfprintf (stdout, "%Y", v1); /* { dg-warning "format" "vfprintf" } */
  vprintf ("%d", v0);
  vprintf ("%Y", v1); /* { dg-warning "format" "vprintf" } */
  vsprintf (s, "%d", v0);
  vsprintf (s, "%Y", v1); /* { dg-warning "format" "vsprintf" } */
  vsnprintf (s, n, "%d", v0);
  vsnprintf (s, n, "%Y", v1); /* { dg-warning "format" "vsnprintf" } */
  printf (gettext ("%d"), i);
  printf (gettext ("%ld"), i);
  printf (dgettext ("", "%d"), i);
  printf (dgettext ("", "%ld"), i);
  printf (dcgettext ("", "%d", 0), i);
  printf (dcgettext ("", "%ld", 0), i);
}
