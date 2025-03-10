/* Test to exercise warnings when an array declared with attribute "nonstring"
   is passed to a function that expects a nul-terminated string as an argument.
   { dg-do compile }
   { dg-options "-O2 -Wattributes -Wstringop-overflow -ftrack-macro-expansion=0" }  */

typedef __SIZE_TYPE__       size_t;
typedef __builtin_va_list   va_list;

#if __cplusplus
extern "C" {
#endif

void* memchr (const void*, int, size_t);
int memcmp (const void*, const void*, size_t);
void* memcpy (void*, const void*, size_t);
void* memmove (void*, const void*, size_t);

int printf (const char*, ...);
int puts (const char*);
int puts_unlocked (const char*);
int sprintf (char*, const char*, ...);
int snprintf (char*, size_t, const char*, ...);
int vsprintf (char*, const char*, va_list);
int vsnprintf (char*, size_t, const char*, va_list);

int strcmp (const char*, const char*);
int strncmp (const char*, const char*, size_t);

char* stpcpy (char*, const char*);
char* stpncpy (char*, const char*, size_t);

char* strcat (char*, const char*);
char* strncat (char*, const char*, size_t);

char* strcpy (char*, const char*);
char* strncpy (char*, const char*, size_t);

char* strchr (const char*, int);
char* strdup (const char*);
size_t strlen (const char*);
size_t strnlen (const char*, size_t);
char* strndup (const char*, size_t);

#if __cplusplus
}   /* extern "C" */
#endif

#define NONSTRING __attribute__ ((nonstring))

/* STR needs to be bigger than ARR to trigger warnings, otherwise
   since STR must be a string, using both in a string function
   can be assumed to be safe even if ARR isn't nul-terminated.  */
char str[3][5];
char arr[3][4] NONSTRING;

char (*ptr)[6];
char (*parr)[6] NONSTRING;

struct MemArrays
{
  char str[5][5];
  char arr[4][4] NONSTRING;
  char (*parr)[4] NONSTRING;
};

void sink (int, ...);


#define T(call)  sink (0, call)

void test_printf (struct MemArrays *p)
{
  T (printf (str[2]));
  T (printf (arr[2]));             /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (printf (*ptr));
  T (printf (*parr));            /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (printf (p->str[2]));
  T (printf (p->arr[2]));          /* { dg-warning "argument 1 declared attribute .nonstring." } */
}


void test_puts (struct MemArrays *p)
{
  T (puts (str[2]));
  T (puts (arr[2]));               /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (puts (*ptr));
  T (puts (*parr));              /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (puts (p->str[2]));
  T (puts (p->arr[2]));            /* { dg-warning "argument 1 declared attribute .nonstring." } */
}


void test_snprintf (char *d, size_t n, struct MemArrays *p)
{
  T (snprintf (d, n, str[2]));
  T (snprintf (d, n, arr[2]));       /* { dg-warning "argument 3 declared attribute .nonstring." } */

  T (snprintf (d, n, *ptr));
  T (snprintf (d, n, *parr));      /* { dg-warning "argument 3 declared attribute .nonstring." } */

  T (snprintf (d, n, p->str[2]));
  T (snprintf (d, n, p->arr[2]));    /* { dg-warning "argument 3 declared attribute .nonstring." } */
}


void test_sprintf (char *d, struct MemArrays *p)
{
  T (sprintf (d, str[2]));
  T (sprintf (d, arr[2]));           /* { dg-warning "argument 2 declared attribute .nonstring." } */

  T (sprintf (d, *ptr));
  T (sprintf (d, *parr));          /* { dg-warning "argument 2 declared attribute .nonstring." } */

  T (sprintf (d, p->str[2]));
  T (sprintf (d, p->arr[2]));        /* { dg-warning "argument 2 declared attribute .nonstring." } */
}


void test_vsnprintf (char *d, size_t n, struct MemArrays *p, va_list va)
{
  T (vsnprintf (d, n, str[2], va));
  T (vsnprintf (d, n, arr[2], va));  /* { dg-warning "argument 3 declared attribute .nonstring." } */

  T (vsnprintf (d, n, *ptr, va));
  T (vsnprintf (d, n, *parr, va)); /* { dg-warning "argument 3 declared attribute .nonstring." } */

  T (vsnprintf (d, n, p->str[2], va));
  T (vsnprintf (d, n, p->arr[2], va)); /* { dg-warning "argument 3 declared attribute .nonstring." } */
}


void test_vsprintf (char *d, struct MemArrays *p, va_list va)
{
  T (vsprintf (d, str[2], va));
  T (vsprintf (d, arr[2], va));      /* { dg-warning "argument 2 declared attribute .nonstring." } */

  T (vsprintf (d, *ptr, va));
  T (vsprintf (d, *parr, va));     /* { dg-warning "argument 2 declared attribute .nonstring." } */

  T (vsprintf (d, p->str[2], va));
  T (vsprintf (d, p->arr[2], va));   /* { dg-warning "argument 2 declared attribute .nonstring." } */
}


void test_strcmp (struct MemArrays *p)
{
  T (strcmp (str[2], str[2]));
  T (strcmp (str[2], arr[2]));          /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcmp (arr[2], str[2]));          /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strcmp (str[2], *ptr));
  T (strcmp (str[2], *parr));         /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcmp (*parr, str[2]));         /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strcmp (p->str[2], p->arr[2]));    /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcmp (p->arr[2], p->str[2]));    /* { dg-warning "argument 1 declared attribute .nonstring." } */
  T (strcmp (*p->parr, p->str[2]));   /* { dg-warning "argument 1 declared attribute .nonstring." } */
}


void test_strncmp_warn (struct MemArrays *p)
{
  enum { N = sizeof arr[2] };
  T (strncmp (str[2], arr[2], N));
  T (strncmp (arr[2], str[2], N));

  T (strncmp (str[2], arr[2], N + 1));   /* { dg-warning "argument 2 declared attribute .nonstring. is smaller than the specified bound 5" } */
  T (strncmp (arr[2], str[2], N + 1));   /* { dg-warning "argument 1 declared attribute .nonstring. is smaller than the specified bound 5" } */

  T (strncmp (str[2], *parr, N + 1));
  T (strncmp (*parr, str[2], N + 1));

  T (strncmp (p->str[2], p->arr[2], N));
  T (strncmp (p->arr[2], p->str[2], N));
  T (strncmp (*p->parr, p->str[2], N));

  T (strncmp (p->str[2], p->arr[2], N));
  T (strncmp (p->arr[2], p->str[2], N));
  T (strncmp (*p->parr, p->str[2], N));
}


void test_strncmp_nowarn (struct MemArrays *p, size_t n)
{
  T (strncmp (str[2], str[2], n));
  T (strncmp (str[2], arr[2], n));
  T (strncmp (arr[2], str[2], n));

  T (strncmp (str[2], *ptr, n));
  T (strncmp (str[2], *parr, n));
  T (strncmp (*parr, str[2], n));

  T (strncmp (p->str[2], p->arr[2], n));
  T (strncmp (p->arr[2], p->str[2], n));
  T (strncmp (*p->parr, p->str[2], n));
}


void test_stpcpy (struct MemArrays *p)
{
  T (stpcpy (str[2], str[2]));
  T (stpcpy (str[2], arr[2]));          /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (stpcpy (arr[2], str[2]));

  T (stpcpy (str[2], *ptr));
  T (stpcpy (str[2], *parr));         /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (stpcpy (*parr, str[2]));

  T (stpcpy (p->str[2], p->arr[2]));    /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (stpcpy (p->arr[2], p->str[2]));
  T (stpcpy (*p->parr, p->str[2]));
}


void test_stpncpy_nowarn (struct MemArrays *p, unsigned n)
{
  T (stpncpy (str[2], str[2], n));
  T (stpncpy (str[2], arr[2], n));
  T (stpncpy (arr[2], str[2], n));

  T (stpncpy (str[2], *ptr, n));
  T (stpncpy (str[2], *parr, n));
  T (stpncpy (*parr, str[2], n));

  T (stpncpy (p->str[2], p->arr[2], n));
  T (stpncpy (p->arr[2], p->str[2], n));
  T (stpncpy (*p->parr, p->str[2], n));
}


void test_stpncpy_warn (struct MemArrays *p, unsigned n)
{
  enum { N = sizeof arr[2] };

  T (stpncpy (str[2], str[2], N));
  T (stpncpy (str[2], arr[2], N));
  T (stpncpy (arr[2], str[2], N));

  T (stpncpy (str[2], *ptr, N));
  T (stpncpy (str[2], *parr, N));
  T (stpncpy (*parr, str[2], N));

  T (stpncpy (p->str[2], p->arr[2], N));
  T (stpncpy (p->arr[2], p->str[2], N));
  T (stpncpy (*p->parr, p->str[2], N));

  T (stpncpy (*ptr, str[2], N + 1));
  T (stpncpy (*ptr, arr[2], N + 1));          /* { dg-warning "argument 2 declared attribute .nonstring. is smaller than the specified bound 5" } */
  T (stpncpy (arr[2], str[2], N + 1));          /* { dg-warning "writing 5 bytes into a region of size 4 overflows " } */

  T (stpncpy (*ptr, *ptr, N + 1));
  T (stpncpy (*ptr, *parr, N + 1));
  T (stpncpy (*parr, str[2], N + 1));

  T (stpncpy (*ptr, p->arr[2], N + 1));       /* { dg-warning "argument 2 declared attribute .nonstring. is smaller" } */
  T (stpncpy (p->arr[2], p->str[2], N + 1));    /* { dg-warning "writing 5 bytes into a region of size 4 overflows " } */
  T (stpncpy (*p->parr, p->str[2], N + 1));
}


void test_strcat (struct MemArrays *p)
{
  T (strcat (str[2], str[2]));
  T (strcat (str[2], arr[2]));          /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcat (arr[2], str[2]));

  T (strcat (str[2], *ptr));
  T (strcat (str[2], *parr));         /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcat (*parr, str[2]));

  T (strcat (p->str[2], p->arr[2]));    /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcat (p->arr[2], p->str[2]));
  T (strcat (*p->parr, p->str[2]));
}


void test_strncat (struct MemArrays *p, unsigned n)
{
  T (strncat (str[2], str[2], n));
  T (strncat (str[2], arr[2], n));      /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strncat (arr[2], str[2], n));

  T (strncat (str[2], *ptr, n));
  T (strncat (str[2], *parr, n));     /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strncat (*parr, str[2], n));

  T (strncat (p->str[2], p->arr[2], n));   /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strncat (p->arr[2], p->str[2], n));
  T (strncat (*p->parr, p->str[2], n));
}


void test_strcpy (struct MemArrays *p)
{
  T (strcpy (str[2], str[2]));
  T (strcpy (str[2], arr[2]));          /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcpy (arr[2], str[2]));

  T (strcpy (str[2], *ptr));
  T (strcpy (str[2], *parr));         /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcpy (*parr, str[2]));

  T (strcpy (p->str[2], p->arr[2]));    /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcpy (p->arr[2], p->str[2]));
  T (strcpy (*p->parr, p->str[2]));
}


void test_strncpy (struct MemArrays *p, unsigned n)
{
  T (strncpy (str[2], str[2], n));
  T (strncpy (str[2], arr[2], n));
  T (strncpy (arr[2], str[2], n));

  T (strncpy (str[2], *ptr, n));
  T (strncpy (str[2], *parr, n));
  T (strncpy (*parr, str[2], n));

  T (strncpy (p->str[2], p->arr[2], n));
  T (strncpy (p->arr[2], p->str[2], n));
  T (strncpy (*p->parr, p->str[2], n));
}


void test_strchr (struct MemArrays *p, int c)
{
  T (strchr (str[2], c));
  T (strchr (arr[2], c));          /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strchr (*ptr, c));
  T (strchr (*parr, c));         /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strchr (p->str[2], c));
  T (strchr (p->arr[2], c));       /* { dg-warning "argument 1 declared attribute .nonstring." } */
}


void test_strdup (struct MemArrays *p)
{
  T (strdup (str[2]));
  T (strdup (arr[2]));             /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strdup (*ptr));
  T (strdup (*parr));            /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strdup (p->str[2]));
  T (strdup (p->arr[2]));          /* { dg-warning "argument 1 declared attribute .nonstring." } */
}


void test_stnrdup_nowarn (struct MemArrays *p, size_t n)
{
  T (strndup (str[2], n));
  T (strndup (arr[2], n));

  T (strndup (*ptr, n));
  T (strndup (*parr, n));

  T (strndup (p->str[2], n));
  T (strndup (p->arr[2], n));
}


void test_stnrdup_warn (struct MemArrays *p)
{
  enum { N = sizeof arr[2] };

  T (strndup (str[2], N));
  T (strndup (arr[2], N));

  T (strndup (*ptr, N));
  T (strndup (*parr, N));

  T (strndup (p->str[2], N));
  T (strndup (p->arr[2], N));


  T (strndup (arr[2], N + 1));     /* { dg-warning "argument 1 declared attribute 'nonstring' is smaller than the specified bound 5|specified bound 5 exceeds source size 4" } */
  T (strndup (*parr, N + 1));
  T (strndup (p->arr[2], N + 1));  /* { dg-warning "argument 1 declared attribute 'nonstring' is smaller than the specified bound 5|specified bound 5 exceeds source size 4" } */
  T (strndup (*p->parr, N + 1));
}


void test_strlen (struct MemArrays *p, char *s NONSTRING, size_t n)
{
  T (strlen (str[2]));
  T (strlen (arr[2]));             /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strlen (*ptr));
  T (strlen (*parr));            /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strlen (p->str[2]));
  T (strlen (p->arr[2]));          /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strlen (s));               /* { dg-warning "argument 1 declared attribute .nonstring." } */
  {
    strcpy (s, "123");
    T (strlen (s));
  }

  {
    char a[][3] __attribute__ ((nonstring)) = { { 1, 2, 3 } };

    T (strlen (a[0]));             /* { dg-warning "argument 1 declared attribute .nonstring." } */
  }

  {
    char a[][4] __attribute__ ((nonstring)) = { { 1, 2, 3, 4 } };

    strcpy (a[0], "12");
    T (strlen (a[0]));
  }
}


void test_strnlen (struct MemArrays *p, size_t n)
{
  T (strnlen (str[2], n));
  T (strnlen (arr[2], n));

  T (strnlen (*ptr, n));
  T (strnlen (*parr, n));

  T (strnlen (p->str[2], n));
  T (strnlen (p->arr[2], n));
}


/* Verify no warnings are issued for raw mempory functions.  */

void test_mem_functions (struct MemArrays *p, int c, size_t n)
{
  T (memchr (arr[2], c, n));
  T (memchr (*parr, c, n));
  T (memchr (p->arr[2], c, n));
  T (memchr (*p->parr, c, n));

  T (memcmp (str[2], arr[2], n));
  T (memcmp (arr[2], str[2], n));
  T (memcmp (str[2], *parr, n));
  T (memcmp (*parr, str[2], n));
  T (memcmp (p->str[2], p->arr[2], n));
  T (memcmp (p->arr[2], p->str[2], n));
  T (memcmp (*p->parr, p->str[2], n));

  T (memcpy (str[2], arr[2], n));
  T (memcpy (arr[2], str[2], n));
  T (memcpy (str[2], *parr, n));
  T (memcpy (*parr, str[2], n));
  T (memcpy (p->str[2], p->arr[2], n));
  T (memcpy (p->arr[2], p->str[2], n));
  T (memcpy (*p->parr, p->str[2], n));

  T (memmove (str[2], arr[2], n));
  T (memmove (arr[2], str[2], n));
  T (memmove (str[2], *parr, n));
  T (memmove (*parr, str[2], n));
  T (memmove (p->str[2], p->arr[2], n));
  T (memmove (p->arr[2], p->str[2], n));
  T (memmove (*p->parr, p->str[2], n));
}
