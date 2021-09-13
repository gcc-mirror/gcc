/* PR middle-end/97631 - bogus "writing one too many bytes" warning for
   memcpy with strlen argument
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

#define NOIPA __attribute__ ((noipa))

typedef __SIZE_TYPE__ size_t;

extern void* malloc (size_t);
extern void* memcpy (void*, const void*, size_t);
extern void* memmove (void*, const void*, size_t);
extern void* memset (void*, int, size_t);
extern char* strcpy (char*, const char*);
extern char* strncpy (char*, const char*, size_t);
extern size_t strlen (const char*);


NOIPA char* nowarn_strcpy (char *s)
{
  size_t n = strlen (s);
  char *d = malloc (n + 1);
  strcpy (d, s);
  return d;
}


NOIPA char* warn_strcpy (char *s)
{
  size_t n = strlen (s);
  char *d = malloc (n);
  strcpy (d, s);        // { dg-warning "\\\[-Wstringop-overflow" }
  return d;
}

NOIPA char* warn_strcpy_nz (char *s)
{
  size_t n = strlen (s);
  if (n == 0)
    return 0;

  char *d = malloc (n);
  strcpy (d, s);        // { dg-warning "\\\[-Wstringop-overflow" }
  return d;
}

NOIPA char* warn_strcpy_nn (char *s)
{
  size_t n = strlen (s);
  char *d = malloc (n);
  if (!d)
    return 0;

  strcpy (d, s);        // { dg-warning "\\\[-Wstringop-overflow" }
  return d;
}

NOIPA char* warn_strcpy_nz_nn (char *s)
{
  size_t n = strlen (s);
  if (n == 0)
    return 0;

  char *d = malloc (n);
  if (!d)
    return 0;

  strcpy (d, s);        // { dg-warning "\\\[-Wstringop-overflow" }
  return d;
}


NOIPA char* nowarn_strncpy_1 (char *s)
{
  /* There's no overflow or truncation below so verify there is no
     warning either.  */
  size_t n = strlen (s) + 1;
  char *d = malloc (n);
  strncpy (d, s, n);
  return d;
}


NOIPA char* warn_strncpy (char *s)
{
  size_t n = strlen (s);
  char *d = malloc (n);
  strncpy (d, s, n);    // { dg-warning "\\\[-Wstringop-truncation" }
  return d;
}

NOIPA char* warn_strncpy_p1 (char *s)
{
  size_t n = strlen (s);
  char *d = malloc (n + 1);
  strncpy (d, s, n);    // { dg-warning "\\\[-Wstringop-truncation" }
  return d;
}

NOIPA char* warn_strncpy_nz (char *s)
{
  size_t n = strlen (s);
  if (n == 0)
    return 0;

  char *d = malloc (n);
  strncpy (d, s, n);    // { dg-warning "\\\[-Wstringop-truncation" }
  return d;

}


NOIPA char* nowarn_memcpy (char *s)
{
  size_t n = strlen (s);
  char *d = malloc (n);
  memcpy (d, s, n);     // { dg-bogus "\\\[-Wstringop-overflow" }
  return d;
}

NOIPA char* nowarn_memcpy_nz (char *s)
{
  size_t n = strlen (s);
  if (n == 0)
    return 0;

  char *d = malloc (n);
  memcpy (d, s, n);     // { dg-bogus "\\\[-Wstringop-overflow" }
  return d;
}

NOIPA char* nowarn_memcpy_nn (char *s)
{
  size_t n = strlen (s);
  char *d = malloc (n);
  if (!d)
    return 0;

  memcpy (d, s, n);     // { dg-bogus "\\\[-Wstringop-overflow" }
  return d;
}

NOIPA char* nowarn_memcpy_nn_nz (char *s)
{
  size_t n = strlen (s);
  if (n == 0)
    return 0;

  char *d = malloc (n);
  if (!d)
    return 0;

  memcpy (d, s, n);     // { dg-bogus "\\\[-Wstringop-overflow" }
  return d;

}


NOIPA char* nowarn_memmove (char *s)
{
  size_t n = strlen (s);
  if (n == 0)
    return 0;

  char *d = malloc (n);
  memmove (d, s, n);    // { dg-bogus "\\\[-Wstringop-overflow" }
  return d;
}


NOIPA char* nowarn_memset (char *s, int c)
{
  size_t n = strlen (s);
  if (n == 0)
    return 0;

  char *d = malloc (n);
  memset (d, c, n);     // { dg-bogus "\\\[-Wstringop-overflow" }
  return d;
}
