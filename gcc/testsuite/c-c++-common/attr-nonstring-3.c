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
char str[5];
char arr[4] NONSTRING;

char *ptr;
char *parr NONSTRING;

struct MemArrays
{
  char str[5];
  char arr[4] NONSTRING;
  char *parr NONSTRING;
};

void sink (int, ...);


#define T(call)  sink (0, call)

void test_printf (struct MemArrays *p)
{
  T (printf (str));
  T (printf (arr));             /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (printf (ptr));
  T (printf (parr));            /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (printf (p->str));
  T (printf (p->arr));          /* { dg-warning "argument 1 declared attribute .nonstring." } */
}


void test_puts (struct MemArrays *p)
{
  T (puts (str));
  T (puts (arr));               /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (puts (ptr));
  T (puts (parr));              /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (puts (p->str));
  T (puts (p->arr));            /* { dg-warning "argument 1 declared attribute .nonstring." } */
}


void test_snprintf (char *d, size_t n, struct MemArrays *p)
{
  T (snprintf (d, n, str));
  T (snprintf (d, n, arr));       /* { dg-warning "argument 3 declared attribute .nonstring." } */

  T (snprintf (d, n, ptr));
  T (snprintf (d, n, parr));      /* { dg-warning "argument 3 declared attribute .nonstring." } */

  T (snprintf (d, n, p->str));
  T (snprintf (d, n, p->arr));    /* { dg-warning "argument 3 declared attribute .nonstring." } */
}


void test_sprintf (char *d, struct MemArrays *p)
{
  T (sprintf (d, str));
  T (sprintf (d, arr));           /* { dg-warning "argument 2 declared attribute .nonstring." } */

  T (sprintf (d, ptr));
  T (sprintf (d, parr));          /* { dg-warning "argument 2 declared attribute .nonstring." } */

  T (sprintf (d, p->str));
  T (sprintf (d, p->arr));        /* { dg-warning "argument 2 declared attribute .nonstring." } */
}


void test_vsnprintf (char *d, size_t n, struct MemArrays *p, va_list va)
{
  T (vsnprintf (d, n, str, va));
  T (vsnprintf (d, n, arr, va));  /* { dg-warning "argument 3 declared attribute .nonstring." } */

  T (vsnprintf (d, n, ptr, va));
  T (vsnprintf (d, n, parr, va)); /* { dg-warning "argument 3 declared attribute .nonstring." } */

  T (vsnprintf (d, n, p->str, va));
  T (vsnprintf (d, n, p->arr, va)); /* { dg-warning "argument 3 declared attribute .nonstring." } */
}


void test_vsprintf (char *d, struct MemArrays *p, va_list va)
{
  T (vsprintf (d, str, va));
  T (vsprintf (d, arr, va));      /* { dg-warning "argument 2 declared attribute .nonstring." } */

  T (vsprintf (d, ptr, va));
  T (vsprintf (d, parr, va));     /* { dg-warning "argument 2 declared attribute .nonstring." } */

  T (vsprintf (d, p->str, va));
  T (vsprintf (d, p->arr, va));   /* { dg-warning "argument 2 declared attribute .nonstring." } */
}


void test_strcmp (struct MemArrays *p)
{
  T (strcmp (str, str));
  T (strcmp (str, arr));          /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcmp (arr, str));          /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strcmp (str, ptr));
  T (strcmp (str, parr));         /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcmp (parr, str));         /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strcmp (p->str, p->arr));    /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcmp (p->arr, p->str));    /* { dg-warning "argument 1 declared attribute .nonstring." } */
  T (strcmp (p->parr, p->str));   /* { dg-warning "argument 1 declared attribute .nonstring." } */
}


void test_strncmp_warn (struct MemArrays *p)
{
  enum { N = sizeof arr };
  T (strncmp (str, arr, N));
  T (strncmp (arr, str, N));

  T (strncmp (str, arr, N + 1));   /* { dg-warning "argument 2 declared attribute .nonstring. is smaller than the specified bound 5" } */
  T (strncmp (arr, str, N + 1));   /* { dg-warning "argument 1 declared attribute .nonstring. is smaller than the specified bound 5" } */

  T (strncmp (str, parr, N + 1));
  T (strncmp (parr, str, N + 1));

  T (strncmp (p->str, p->arr, N));
  T (strncmp (p->arr, p->str, N));
  T (strncmp (p->parr, p->str, N));

  T (strncmp (p->str, p->arr, N));
  T (strncmp (p->arr, p->str, N));
  T (strncmp (p->parr, p->str, N));
}


void test_strncmp_nowarn (struct MemArrays *p, size_t n)
{
  T (strncmp (str, str, n));
  T (strncmp (str, arr, n));
  T (strncmp (arr, str, n));

  T (strncmp (str, ptr, n));
  T (strncmp (str, parr, n));
  T (strncmp (parr, str, n));

  T (strncmp (p->str, p->arr, n));
  T (strncmp (p->arr, p->str, n));
  T (strncmp (p->parr, p->str, n));
}


void test_stpcpy (struct MemArrays *p)
{
  T (stpcpy (str, str));
  T (stpcpy (str, arr));          /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (stpcpy (arr, str));

  T (stpcpy (str, ptr));
  T (stpcpy (str, parr));         /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (stpcpy (parr, str));

  T (stpcpy (p->str, p->arr));    /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (stpcpy (p->arr, p->str));
  T (stpcpy (p->parr, p->str));
}


void test_stpncpy_nowarn (struct MemArrays *p, unsigned n)
{
  T (stpncpy (str, str, n));
  T (stpncpy (str, arr, n));
  T (stpncpy (arr, str, n));

  T (stpncpy (str, ptr, n));
  T (stpncpy (str, parr, n));
  T (stpncpy (parr, str, n));

  T (stpncpy (p->str, p->arr, n));
  T (stpncpy (p->arr, p->str, n));
  T (stpncpy (p->parr, p->str, n));
}


void test_stpncpy_warn (struct MemArrays *p, unsigned n)
{
  enum { N = sizeof arr };

  T (stpncpy (str, str, N));
  T (stpncpy (str, arr, N));
  T (stpncpy (arr, str, N));

  T (stpncpy (str, ptr, N));
  T (stpncpy (str, parr, N));
  T (stpncpy (parr, str, N));

  T (stpncpy (p->str, p->arr, N));
  T (stpncpy (p->arr, p->str, N));
  T (stpncpy (p->parr, p->str, N));

  T (stpncpy (ptr, str, N + 1));
  T (stpncpy (ptr, arr, N + 1));          /* { dg-warning "argument 2 declared attribute .nonstring. is smaller than the specified bound 5" } */
  T (stpncpy (arr, str, N + 1));          /* { dg-warning "writing 5 bytes into a region of size 4 overflows " } */

  T (stpncpy (ptr, ptr, N + 1));
  T (stpncpy (ptr, parr, N + 1));
  T (stpncpy (parr, str, N + 1));

  T (stpncpy (ptr, p->arr, N + 1));       /* { dg-warning "argument 2 declared attribute .nonstring. is smaller" } */
  T (stpncpy (p->arr, p->str, N + 1));    /* { dg-warning "writing 5 bytes into a region of size 4 overflows " } */
  T (stpncpy (p->parr, p->str, N + 1));
}


void test_strcat (struct MemArrays *p)
{
  T (strcat (str, str));
  T (strcat (str, arr));          /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcat (arr, str));

  T (strcat (str, ptr));
  T (strcat (str, parr));         /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcat (parr, str));

  T (strcat (p->str, p->arr));    /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcat (p->arr, p->str));
  T (strcat (p->parr, p->str));
}


void test_strncat (struct MemArrays *p, unsigned n)
{
  T (strncat (str, str, n));
  T (strncat (str, arr, n));      /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strncat (arr, str, n));

  T (strncat (str, ptr, n));
  T (strncat (str, parr, n));     /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strncat (parr, str, n));

  T (strncat (p->str, p->arr, n));   /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strncat (p->arr, p->str, n));
  T (strncat (p->parr, p->str, n));
}


void test_strcpy (struct MemArrays *p)
{
  T (strcpy (str, str));
  T (strcpy (str, arr));          /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcpy (arr, str));

  T (strcpy (str, ptr));
  T (strcpy (str, parr));         /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcpy (parr, str));

  T (strcpy (p->str, p->arr));    /* { dg-warning "argument 2 declared attribute .nonstring." } */
  T (strcpy (p->arr, p->str));
  T (strcpy (p->parr, p->str));
}


void test_strncpy (struct MemArrays *p, unsigned n)
{
  T (strncpy (str, str, n));
  T (strncpy (str, arr, n));
  T (strncpy (arr, str, n));

  T (strncpy (str, ptr, n));
  T (strncpy (str, parr, n));
  T (strncpy (parr, str, n));

  T (strncpy (p->str, p->arr, n));
  T (strncpy (p->arr, p->str, n));
  T (strncpy (p->parr, p->str, n));
}


void test_strchr (struct MemArrays *p, int c)
{
  T (strchr (str, c));
  T (strchr (arr, c));          /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strchr (ptr, c));
  T (strchr (parr, c));         /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strchr (p->str, c));
  T (strchr (p->arr, c));       /* { dg-warning "argument 1 declared attribute .nonstring." } */
}


void test_strdup (struct MemArrays *p)
{
  T (strdup (str));
  T (strdup (arr));             /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strdup (ptr));
  T (strdup (parr));            /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strdup (p->str));
  T (strdup (p->arr));          /* { dg-warning "argument 1 declared attribute .nonstring." } */
}


void test_stnrdup_nowarn (struct MemArrays *p, size_t n)
{
  T (strndup (str, n));
  T (strndup (arr, n));

  T (strndup (ptr, n));
  T (strndup (parr, n));

  T (strndup (p->str, n));
  T (strndup (p->arr, n));
}


void test_stnrdup_warn (struct MemArrays *p)
{
  enum { N = sizeof arr };

  T (strndup (str, N));
  T (strndup (arr, N));

  T (strndup (ptr, N));
  T (strndup (parr, N));

  T (strndup (p->str, N));
  T (strndup (p->arr, N));


  T (strndup (arr, N + 1));     /* { dg-warning "argument 1 declared attribute .nonstring. is smaller than the specified bound 5" } */
  T (strndup (parr, N + 1));
  T (strndup (p->arr, N + 1));  /* { dg-warning "argument 1 declared attribute .nonstring. is smaller than the specified bound 5" } */
  T (strndup (p->parr, N + 1));
}


void test_strlen (struct MemArrays *p, char *s NONSTRING, size_t n)
{
  T (strlen (str));
  T (strlen (arr));             /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strlen (ptr));
  T (strlen (parr));            /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strlen (p->str));
  T (strlen (p->arr));          /* { dg-warning "argument 1 declared attribute .nonstring." } */

  T (strlen (s));               /* { dg-warning "argument 1 declared attribute .nonstring." } */
  {
    strcpy (s, "123");
    T (strlen (s));
  }

  {
    char a[] __attribute__ ((nonstring)) = { 1, 2, 3 };

    T (strlen (a));             /* { dg-warning "argument 1 declared attribute .nonstring." } */
  }

  {
    char a[] __attribute__ ((nonstring)) = { 1, 2, 3, 4 };

    strcpy (a, "12");
    T (strlen (a));
  }

  {
    char *p __attribute__ ((nonstring));
    p = (char *)__builtin_malloc (n);
    __builtin_memset (p, '*', n);

    T (strlen (p));             /* { dg-warning "argument 1 declared attribute .nonstring." } */

    strcpy (p, "12345");
    T (strlen (p));
  }
}


void test_strnlen (struct MemArrays *p, size_t n)
{
  T (strnlen (str, n));
  T (strnlen (arr, n));

  T (strnlen (ptr, n));
  T (strnlen (parr, n));

  T (strnlen (p->str, n));
  T (strnlen (p->arr, n));
}


/* Verify no warnings are issued for raw mempory functions.  */

void test_mem_functions (struct MemArrays *p, int c, size_t n)
{
  T (memchr (arr, c, n));
  T (memchr (parr, c, n));
  T (memchr (p->arr, c, n));
  T (memchr (p->parr, c, n));

  T (memcmp (str, arr, n));
  T (memcmp (arr, str, n));
  T (memcmp (str, parr, n));
  T (memcmp (parr, str, n));
  T (memcmp (p->str, p->arr, n));
  T (memcmp (p->arr, p->str, n));
  T (memcmp (p->parr, p->str, n));

  T (memcpy (str, arr, n));
  T (memcpy (arr, str, n));
  T (memcpy (str, parr, n));
  T (memcpy (parr, str, n));
  T (memcpy (p->str, p->arr, n));
  T (memcpy (p->arr, p->str, n));
  T (memcpy (p->parr, p->str, n));

  T (memmove (str, arr, n));
  T (memmove (arr, str, n));
  T (memmove (str, parr, n));
  T (memmove (parr, str, n));
  T (memmove (p->str, p->arr, n));
  T (memmove (p->arr, p->str, n));
  T (memmove (p->parr, p->str, n));
}
