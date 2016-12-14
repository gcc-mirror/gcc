/* PR c/17308 - nonnull attribute not as useful as it could be
   PR c/78673 - sprintf missing attribute nonnull on destination argument
   { dg-do "compile" }
   { dg-additional-options "-O2 -Wnonnull -ftrack-macro-expansion=0 -std=c99" } */

#define va_list __builtin_va_list

typedef struct FILE FILE;

char* null (void)
{
  return 0;
}

void sink (int, ...);
#define T(arg) sink (0, arg)


#define bzero    __builtin_bzero
#define memcpy   __builtin_memcpy
#define memmove  __builtin_memmove
#define mempcpy  __builtin_mempcpy
#define memset   __builtin_memset

void test_memfuncs (void *s, unsigned n)
{
  /* Bzero is not declared attribute nonnull.  */
  bzero (null (), n);

  T (memcpy (null (), s, n));     /* { dg-warning "argument 1 null where non-null expected" } */
  T (memcpy (s, null (), n));     /* { dg-warning "argument 2 null where non-null expected" } */

  T (memmove (null (), s, n));    /* { dg-warning "argument 1 null where non-null expected" } */
  T (memmove (s, null (), n));    /* { dg-warning "argument 2 null where non-null expected" } */

  T (mempcpy (null (), s, n));    /* { dg-warning "argument 1 null where non-null expected" } */
  T (mempcpy (s, null (), n));    /* { dg-warning "argument 2 null where non-null expected" } */

  T (memset (null (), 0, n));     /* { dg-warning "argument 1 null where non-null expected" } */
}

#undef memcpy
#undef memmove
#undef mempcpy
#undef memset
#define memcpy(d, s, n)   __builtin___memcpy_chk (d, s, n, n)
#define memmove(d, s, n)  __builtin___memmove_chk (d, s, n, n)
#define mempcpy(d, s, n)  __builtin___mempcpy_chk (d, s, n, n)
#define memset(d, x, n)   __builtin___memset_chk (d, x, n, n)

void test_memfuncs_chk (void *s, unsigned n)
{
  T (memcpy (null (), s, n));     /* { dg-warning "argument 1 null where non-null expected" } */
  T (memcpy (s, null (), n));     /* { dg-warning "argument 2 null where non-null expected" } */

  T (memmove (null (), s, n));    /* { dg-warning "argument 1 null where non-null expected" } */
  T (memmove (s, null (), n));    /* { dg-warning "argument 2 null where non-null expected" } */

  T (mempcpy (null (), s, n));    /* { dg-warning "argument 1 null where non-null expected" } */
  T (mempcpy (s, null (), n));    /* { dg-warning "argument 2 null where non-null expected" } */

  T (memset (null (), 0, n));     /* { dg-warning "argument 1 null where non-null expected" } */
}


#define strcat   __builtin_strcat
#define strchr   __builtin_strchr
#define stpcpy   __builtin_stpcpy
#define stpncpy  __builtin_stpncpy
#define strcpy   __builtin_strcpy
#define strncpy  __builtin_strncpy
#define strlen   __builtin_strlen
#define strncat  __builtin_strncat
#define strstr   __builtin_strstr

void test_strfuncs (char *s, unsigned n)
{
  T (strcat (null (), s));        /* { dg-warning "argument 1 null where non-null expected" } */
  T (strcat (s, null ()));        /* { dg-warning "argument 2 null where non-null expected" } */

  T (strchr (null (), 'x'));      /* { dg-warning "argument 1 null where non-null expected" } */

  T (stpcpy (null (), s));        /* { dg-warning "argument 1 null where non-null expected" } */
  T (stpcpy (s, null ()));        /* { dg-warning "argument 2 null where non-null expected" } */

  T (stpncpy (null (), s, n));    /* { dg-warning "argument 1 null where non-null expected" } */
  T (stpncpy (s, null (), n));    /* { dg-warning "argument 2 null where non-null expected" } */

  T (strcpy (null (), s));        /* { dg-warning "argument 1 null where non-null expected" } */
  T (strcpy (s, null ()));        /* { dg-warning "argument 2 null where non-null expected" } */

  T (strncpy (null (), s, n));    /* { dg-warning "argument 1 null where non-null expected" } */
  T (strncpy (s, null (), n));    /* { dg-warning "argument 2 null where non-null expected" } */

  T (strlen (null ()));           /* { dg-warning "argument 1 null where non-null expected" } */

  T (strncat (s, null (), n));    /* { dg-warning "argument 2 null where non-null expected" } */
  T (strncat (null (), s, n));    /* { dg-warning "argument 1 null where non-null expected" } */

  T (strstr (null (), s));        /* { dg-warning "argument 1 null where non-null expected" } */
  T (strstr (s, null ()));        /* { dg-warning "argument 2 null where non-null expected" } */
}


#undef strcat
#undef stpcpy
#undef stpncpy
#undef strcpy
#undef strncpy
#undef strncat

#define strcat(d, s)      __builtin___strcat_chk (d, s, n)
#define stpcpy(d, s)      __builtin___stpcpy_chk (d, s, n)
#define stpncpy(d, s, n)  __builtin___stpncpy_chk (d, s, n, n)
#define strcpy(d, s)      __builtin___strcpy_chk (d, s, n)
#define strncpy(d, s, n)  __builtin___strncpy_chk (d, s, n, n)
#define strncat(d, s, n)  __builtin___strncat_chk (d, s, n, n)

void test_strfuncs_chk (char *s, unsigned n)
{
  T (strcat (null (), s));        /* { dg-warning "argument 1 null where non-null expected" } */
  T (strcat (s, null ()));        /* { dg-warning "argument 2 null where non-null expected" } */

  T (strchr (null (), 'x'));      /* { dg-warning "argument 1 null where non-null expected" } */

  T (stpcpy (null (), s));        /* { dg-warning "argument 1 null where non-null expected" } */
  T (stpcpy (s, null ()));        /* { dg-warning "argument 2 null where non-null expected" } */

  T (stpncpy (null (), s, n));    /* { dg-warning "argument 1 null where non-null expected" } */
  T (stpncpy (s, null (), n));    /* { dg-warning "argument 2 null where non-null expected" } */

  T (strcpy (null (), s));        /* { dg-warning "argument 1 null where non-null expected" } */
  T (strcpy (s, null ()));        /* { dg-warning "argument 2 null where non-null expected" } */

  T (strncpy (null (), s, n));    /* { dg-warning "argument 1 null where non-null expected" } */
  T (strncpy (s, null (), n));    /* { dg-warning "argument 2 null where non-null expected" } */

  T (strncat (s, null (), n));    /* { dg-warning "argument 2 null where non-null expected" } */
  T (strncat (null (), s, n));    /* { dg-warning "argument 1 null where non-null expected" } */
}


#define fprintf             __builtin_fprintf
#define fprintf_unlocked    __builtin_fprintf_unlocked
#define vfprintf            __builtin_vfprintf
#define printf              __builtin_printf
#define printf_unlocked     __builtin_printf_unlocked
#define vprintf             __builtin_vprintf
#define sprintf             __builtin_sprintf
#define snprintf            __builtin_snprintf
#define vsprintf            __builtin_vsprintf
#define vsnprintf           __builtin_vsnprintf

void test_stdio_funcs (FILE *f, char *d, unsigned n, va_list va)
{
  T (fprintf (null (), "%i", 0)); /* { dg-warning "argument 1 null where non-null expected" } */
  T (fprintf (f, null ()));       /* { dg-warning "argument 2 null where non-null expected" } */

  T (fprintf_unlocked (null (), "%i", 0)); /* { dg-warning "argument 1 null where non-null expected" } */
  T (fprintf_unlocked (f, null ()));       /* { dg-warning "argument 2 null where non-null expected" } */

  T (vfprintf (null (), "%i", va));/* { dg-warning "argument 1 null where non-null expected" } */
  T (vfprintf (f, null (), va));   /* { dg-warning "argument 2 null where non-null expected" } */

  T (vprintf (null (), va));      /* { dg-warning "argument 1 null where non-null expected" } */

  T (printf (null ()));           /* { dg-warning "argument 1 null where non-null expected" } */
  T (printf_unlocked (null ()));  /* { dg-warning "argument 1 null where non-null expected" } */

  T (vprintf (null (), va));      /* { dg-warning "argument 1 null where non-null expected" } */

  T (sprintf (null (), "%i", 0)); /* { dg-warning "argument 1 null where non-null expected" } */
  T (sprintf (d, null ()));       /* { dg-warning "argument 2 null where non-null expected" } */

  T (snprintf (null (), n, "%i", 0));
  T (snprintf (d, n, null ()));   /* { dg-warning "argument 3 null where non-null expected" } */

  T (vsprintf (null (), "%i", va)); /* { dg-warning "argument 1 null where non-null expected" } */
  T (vsprintf (d, null (), va));   /* { dg-warning "argument 2 null where non-null expected" } */

  T (vsnprintf (null (), n, "%i", va));
  T (vsnprintf (d, n, null (), va));  /* { dg-warning "argument 3 null where non-null expected" } */
}

#undef fprintf
#undef fprintf_unlocked
#undef vfprintf
#undef printf
#undef printf_unlocked
#undef vprintf
#undef sprintf
#undef snprintf
#undef vsprintf
#undef vsnprintf

#define fprintf(f, fmt, ...)				\
  __builtin___fprintf_chk (f, 0, fmt, __VA_ARGS__)
#define vfprintf(f, fmt, va)			\
  __builtin___vfprintf_chk (f, 0, fmt, va)
#define printf(fmt, ...)			\
  __builtin___printf_chk (0, fmt, __VA_ARGS__)
#define vprintf(fmt, va)			\
  __builtin___vprintf_chk (0, fmt, va)
#define sprintf(d, fmt, ... )				\
  __builtin___sprintf_chk (d, 0, n, fmt, __VA_ARGS__)
#define snprintf(d, n, fmt, ...)			\
  __builtin___snprintf_chk (d, n, 0, n,  fmt, __VA_ARGS__)
#define vsprintf(d, fmt, va)			\
  __builtin___vsprintf_chk (d, 0, n, fmt, va)
#define vsnprintf(d, n, fmt, va)			\
  __builtin___vsnprintf_chk (d, n, 0, n, fmt, va)

void test_stdio_funcs_chk (FILE *f, char *d, const char *fmt,
			   unsigned n, va_list va)
{
  T (fprintf (null (), "%i", 0)); /* { dg-warning "argument 1 null where non-null expected" } */
  T (fprintf (f, null (), 0));    /* { dg-warning "argument 3 null where non-null expected" } */

  T (vfprintf (null (), "%i", va));/* { dg-warning "argument 1 null where non-null expected" } */
  T (vfprintf (f, null (), va));   /* { dg-warning "argument 3 null where non-null expected" } */

  T (vprintf (null (), va));      /* { dg-warning "argument 2 null where non-null expected" } */

  T (printf (null (), 0));        /* { dg-warning "argument 2 null where non-null expected" } */

  T (vprintf (null (), va));      /* { dg-warning "argument 2 null where non-null expected" } */

  T (sprintf (null (), "%i", 0)); /* { dg-warning "argument 1 null where non-null expected" } */
  T (sprintf (d, null (), 0));    /* { dg-warning "argument 4 null where non-null expected" } */

  T (snprintf (null (), n, "%i", 0));
  T (snprintf (d, n, null (), 0));  /* { dg-warning "argument 5 null where non-null expected" } */

  T (vsprintf (null (), "%i", va)); /* { dg-warning "argument 1 null where non-null expected" } */
  T (vsprintf (d, null (), va));   /* { dg-warning "argument 4 null where non-null expected" } */

  T (vsnprintf (null (), n, "%i", va));
  T (vsnprintf (d, n, null (), va));  /* { dg-warning "argument 5 null where non-null expected" } */
}
