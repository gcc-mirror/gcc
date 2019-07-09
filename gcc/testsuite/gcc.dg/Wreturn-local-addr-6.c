/* PR c/71924 - missing -Wreturn-local-addr returning alloca result
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

void* memcpy (void*, const void*, size_t);
void* mempcpy (void*, const void*, size_t);
void* memmove (void*, const void*, size_t);

char* stpcpy (char*, const char*);
char* stpncpy (char*, const char*, size_t);

size_t strlen (const char*);
size_t strnlen (const char*, size_t);

char* strcat (char*, const char*);
char* strncat (char*, const char*, size_t);

char* strcpy (char*, const char*);
char* strncpy (char*, const char*, size_t);

char* strdup (const char*);

char* strchr (const char*, int);
char* strrchr (const char*, int);
char* strstr (const char*, const char*);

void sink (void*, ...);


void* return_memcpy (const void *s, unsigned n)
{
  char a[n];                  /* { dg-message "declared here" } */
  void *p = memcpy (a, s, n);
  sink (p);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */
}

void* return_memcpy_cst (const void *s, unsigned n)
{
  char a[n];                  /* { dg-message "declared here" } */
  void *p = memcpy (a + 1, s, n);
  sink (p);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */
}

void* return_memcpy_var (const void *s, unsigned n, int i)
{
  char a[n];                  /* { dg-message "declared here" } */
  void *p = memcpy (a + i, s, n);
  sink (p);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */
}

void* return_mempcpy (const void *s, unsigned n)
{
  char a[n];                  /* { dg-message "declared here" } */
  void *p = mempcpy (a, s, n);
  sink (p);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */
}

void* return_memmove_cst (unsigned n)
{
  char a[n];                  /* { dg-message "declared here" } */
  sink (a);
  void *p = memmove (a + 1, a, n);
  sink (p);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */
}

void* return_memmove_var (unsigned n, int i)
{
  char a[n];                  /* { dg-message "declared here" } */
  sink (a);
  void *p = memmove (a + i, a, n);
  sink (p);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */
}

char* return_stpcpy (unsigned n, const char *s)
{
  char a[n];                  /* { dg-message "declared here" } */
  char *p = stpcpy (a, s);
  sink (p);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */
}

char* return_stpncpy (unsigned n, const char *s)
{
  char a[n];                  /* { dg-message "declared here" } */
  char *p = stpncpy (a, s, n);
  sink (p);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */
}

char* return_strcat (unsigned n, const char *s)
{
  char a[n];                  /* { dg-message "declared here" } */
  sink (a);
  char *p = strcat (a, s);
  sink (p);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */
}

char* return_strncat (unsigned n, const char *s)
{
  char a[n];                  /* { dg-message "declared here" } */
  sink (a);
  char *p = strncat (a, s, n);
  sink (p);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */

}
char* return_strcpy (unsigned n, const char *s)
{
  char a[n];                  /* { dg-message "declared here" } */
  char *p = strcpy (a, s);
  sink (p);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */
}

char* return_strcpy_plus_strlen (unsigned n, const char *s)
{
  char a[n];                  /* { dg-message "declared here" } */
  char *p = strcpy (a, s);
  sink (p);
  p += strlen (p);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */
}

char* return_strcpy_cst_plus_strlen (unsigned n, const char *s)
{
  char a[n];                  /* { dg-message "declared here" } */
  sink (a);
  char *p = strcpy (a + 1, s);
  sink (p);
  p += strlen (p);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */
}

char* return_strcpy_var_plus_strlen (unsigned n, const char *s, int i)
{
  char a[n];                  /* { dg-message "declared here" } */
  sink (a);
  char *p = strcpy (a + i, s);
  sink (p);
  p += strlen (p);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */
}

char* return_strncpy (unsigned n, const char *s)
{
  char a[n];                  /* { dg-message "declared here" } */
  char *p = strncpy (a, s, n);
  sink (p);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */
}

char* return_strncpy_plus_strnlen (unsigned n, const char *s)
{
  char a[n];                  /* { dg-message "declared here" } */
  char *p = strncpy (a, s, n);
  p += strnlen (p, n);
  sink (p);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */
}

char* return_strdup (unsigned n)
{
  char a[n];
  sink (a);
  char *p = strdup (a);
  return p;
}

char* return_strchr (unsigned n, int c)
{
  char a[n];                  /* { dg-message "declared here" } */
  sink (a);
  char *p = strchr (a, c);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */
}

char* return_strstr (unsigned n, const char *s)
{
  char a[n];                  /* { dg-message "declared here" } */
  sink (a);
  char *p = strstr (a, s);
  if (p)
    p += strlen (p);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */
}

char* return_strrchr (unsigned n, int c)
{
  char a[n];                  /* { dg-message "declared here" } */
  sink (a);
  char *p = strrchr (a, c);
  return p;                   /* { dg-warning "\\\[-Wreturn-local-addr]" } */

}
