/* Test for scanf formats.  Test that the C99 functions get their default
   attributes in strict C99 mode, but the gettext functions do not.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic -Wformat" } */

/* This may not be correct in the particular case, but allows the
   prototypes to be declared, and we don't try to link.
*/
typedef struct _FILE FILE;
extern FILE *stdin;

typedef __builtin_va_list va_list;

extern int fscanf (FILE *restrict, const char *restrict, ...);
extern int scanf (const char *restrict, ...);
extern int sscanf (const char *restrict, const char *restrict, ...);
extern int vfscanf (FILE *restrict, const char *restrict, va_list);
extern int vscanf (const char *restrict, va_list);
extern int vsscanf (const char *restrict, const char *restrict, va_list);

extern char *gettext (const char *);
extern char *dgettext (const char *, const char *);
extern char *dcgettext (const char *, const char *, int);

void
foo (int *ip, char *s, va_list v0, va_list v1, va_list v2, va_list v3,
     va_list v4, va_list v5)
{
  fscanf (stdin, "%d", ip);
  fscanf (stdin, "%ld", ip); /* { dg-warning "format" "fscanf" } */
  scanf ("%d", ip);
  scanf ("%ld", ip); /* { dg-warning "format" "scanf" } */
  sscanf (s, "%d", ip);
  sscanf (s, "%ld", ip); /* { dg-warning "format" "sscanf" } */
  vfscanf (stdin, "%d", v0);
  vfscanf (stdin, "%Y", v1); /* { dg-warning "format" "vfscanf" } */
  vscanf ("%d", v2);
  vscanf ("%Y", v3); /* { dg-warning "format" "vscanf" } */
  vsscanf (s, "%d", v4);
  vsscanf (s, "%Y", v5); /* { dg-warning "format" "vsscanf" } */
  scanf (gettext ("%d"), ip);
  scanf (gettext ("%ld"), ip);
  scanf (dgettext ("", "%d"), ip);
  scanf (dgettext ("", "%ld"), ip);
  scanf (dcgettext ("", "%d", 0), ip);
  scanf (dcgettext ("", "%ld", 0), ip);
}
