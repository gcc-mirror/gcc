/* PR middle-end/93926 - ICE on a built-in redeclaration returning an integer
   instead of a pointer
   { dg-do compile }
   { dg-options "-Wall" } */

typedef __SIZE_TYPE__ size_t;

void* ret_calloc (size_t n1, size_t n2)
{
  extern size_t calloc (size_t, size_t);    // { dg-warning "\\\[-Wbuiltin-declaration-mismatch" }

  return (void *) calloc (n1, n2);
}

void* ret_malloc (size_t n)
{
  extern size_t malloc (size_t);            // { dg-warning "\\\[-Wbuiltin-declaration-mismatch" }

  return (void *) malloc (n);
}

void* ret_realloc (void *p, size_t n)
{
  extern size_t realloc (void *p, size_t);  // { dg-warning "\\\[-Wbuiltin-declaration-mismatch" }

  return (void *) realloc (p, n);
}

void* ret_strdup (const char *s)
{
  extern size_t strdup (const char*);       // { dg-warning "\\\[-Wbuiltin-declaration-mismatch" }

  return (void *) strdup (s);
}

void* ret_strndup (const char *s, size_t n)
{
  extern size_t
    strndup (const char*, size_t);          // { dg-warning "\\\[-Wbuiltin-declaration-mismatch" }

  return (void *) strndup (s, n);
}

// For good measure also exerise strcmp return type (not part of the bug).

char* ret_strcmp (const char *s, const char *t)
{
  extern char*
    strcmp (const char*, const char*);      // { dg-warning "\\\[-Wbuiltin-declaration-mismatch" }

  return strcmp (s, t);
}

// Exercise warnings for pointer/integer mismatches in argument types
// (also not part of the bug).

char* ret_strcat (size_t s, const char *t)
{
  extern char*
    strcat (size_t, const char*);           // { dg-warning "\\\[-Wbuiltin-declaration-mismatch" }

  return strcat (s, t);
}

char* ret_strcpy (char *s, size_t t)
{
  extern char* strcpy (char*, size_t);      // { dg-warning "\\\[-Wbuiltin-declaration-mismatch" }

  return strcpy (s, t);
}

char* ret_strncpy (char *s, const char *t, size_t n)
{
  extern char*
    strncpy (char*, size_t, const char*);   // { dg-warning "\\\[-Wbuiltin-declaration-mismatch" }

  return strncpy (s, n, t);
}
