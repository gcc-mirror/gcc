/* PR middle-end/78519 - missing warning for sprintf %s with null pointer
   Also exercises null destination pointer and null format string.
   { dg-do compile }
   { dg-options "-O2 -Wformat -Wformat-overflow -Wno-nonnull -ftrack-macro-expansion=0" } */

typedef __builtin_va_list va_list;

#define sprintf   __builtin_sprintf
#define snprintf  __builtin_snprintf
#define vsprintf  __builtin_vsprintf
#define vsnprintf __builtin_vsnprintf


static char* null (void)
{
  return 0;
}


void sink (int);
#define T sink


/* Verify that calls with a null destination pointer are diagnosed.  */

void test_null_dest (va_list va)
{
  char *p = null ();
  T (sprintf (null (), "%i", 0));   /* { dg-warning "null destination pointer" } */
  T (sprintf (p, "%i", 0));         /* { dg-warning "null destination pointer" } */
  T (sprintf (p, "%i abc", 0));     /* { dg-warning "null destination pointer" } */

  T (snprintf (null (), 1, "%i", 0));   /* { dg-warning "null destination pointer" } */
  T (snprintf (p, 2, "%i", 0));         /* { dg-warning "null destination pointer" } */
  T (snprintf (p, 3, "%i abc", 0));     /* { dg-warning "null destination pointer" } */

  /* Snprintf with a null pointer and a zero size is a special request
     to determine the size of output without writing any.  Such calls
     are valid must not be diagnosed.  */
  T (snprintf (p, 0, "%i", 0));

  T (vsprintf (null (), "%i", va)); /* { dg-warning "null destination pointer" } */
  T (vsprintf (p,       "%i", va)); /* { dg-warning "null destination pointer" } */

  T (vsnprintf (null (), 1, "%i", va)); /* { dg-warning "null destination pointer" } */
  T (vsnprintf (p,       2, "%i", va));       /* { dg-warning "null destination pointer" } */
  T (vsnprintf (p,       0, "%i", va));
}

void test_null_format (char *d, va_list va)
{
  char *fmt = null ();

  T (sprintf (d, null ()));    /* { dg-warning "null format string" } */
  T (sprintf (d, fmt));        /* { dg-warning "null format string" } */

  T (snprintf (d, 0, null ()));    /* { dg-warning "null format string" } */
  T (snprintf (d, 1, fmt));        /* { dg-warning "null format string" } */

  T (vsprintf (d, null (), va));   /* { dg-warning "null format string" } */
  T (vsprintf (d, fmt, va));       /* { dg-warning "null format string" } */

  T (vsnprintf (d, 0, null (), va));  /* { dg-warning "null format string" } */
  T (vsnprintf (d, 1, fmt, va));      /* { dg-warning "null format string" } */
}

void test_null_arg (char *d, const char *s)
{
  char *p = null ();

  T (sprintf (d, "%-s", null ()));  /* { dg-warning "directive argument is null" } */
  T (sprintf (d, "%-s", p));        /* { dg-warning "directive argument is null" } */
  T (sprintf (d, "%s %s", p, s));   /* { dg-warning "directive argument is null" } */
  T (sprintf (d, "%s %s", s, p));   /* { dg-warning "directive argument is null" } */
  T (sprintf (d, "%s %i", p, 1));   /* { dg-warning "directive argument is null" } */
  T (sprintf (d, "%i %s", 1, p));   /* { dg-warning "directive argument is null" } */
  T (sprintf (d, "%.0s", p));       /* { dg-warning "directive argument is null" } */
  T (sprintf (d, "%1.0s", p));      /* { dg-warning "directive argument is null" } */

  T (snprintf (d, 0, "%-s", null ()));  /* { dg-warning "directive argument is null" } */
  T (snprintf (d, 1, "%-s", p));        /* { dg-warning "directive argument is null" } */

  T (sprintf (d, "%i %s", 1, null ()));  /* { dg-warning "directive argument is null" } */
  T (sprintf (d, "%i %s", 2, p));        /* { dg-warning "directive argument is null" } */

  T (snprintf (d, 0, "%i %s", 1, null ()));  /* { dg-warning "directive argument is null" } */
  T (snprintf (d, 9, "%i %s", 2, p));        /* { dg-warning "directive argument is null" } */

  /* A sanity check that the %p directive doesn't emit a warning
     with a null pointer.  */
  T (sprintf (d, "%p", null ()));
  T (sprintf (d, "%p", p));
}
