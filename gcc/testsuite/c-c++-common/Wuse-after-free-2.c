/* Verify that accessing freed objects by built-in functions is diagnosed.
   { dg-do compile }
   { dg-options "-Wall" }  */

typedef __SIZE_TYPE__ size_t;

#if __cplusplus
#  define EXTERN_C extern "C"
#else
#  define EXTERN_C extern
#endif

EXTERN_C void free (void*);
EXTERN_C void* realloc (void*, size_t);

EXTERN_C void* memcpy (void*, const void*, size_t);
EXTERN_C char* strcpy (char*, const char*);
EXTERN_C size_t strlen (const char*);


void sink (void*, ...);

struct Member { char *p; char a[4]; };

int nowarn_strcpy_memptr (struct Member *p)
{
  char *q = strcpy (p->p, p->a);
  free (p);
  return *q;
}

int nowarn_strlen_memptr (struct Member *p)
{
  const char *q = p->p;

  free (p);

  return strlen (q);
}

int warn_strlen_memptr (struct Member *p)
{
  free (p);                   // { dg-message "call to '\(void \)?free\(\\(void\\*\\)\)?'" "note" }
  return strlen (p->p);       // { dg-warning "-Wuse-after-free" }
}

int warn_strlen_memarray (struct Member *p)
{
  {
    free (p);
    return strlen (p->a);     // { dg-warning "-Wuse-after-free" }
  }

  {
    char *q = p->a;

    free (p);
    return strlen (q);        // { dg-warning "-Wuse-after-free" "pr??????" { xfail *-*-* } }
  }
}

void* nowarn_realloc_success (void *p)
{
  void *q = realloc (p, 7);
  if (!q)
    /* When realloc fails the original pointer remains valid.  */
    return p;

  return q;
}

void* nowarn_realloc_equal (void *p, int *moved)
{
  void *q = realloc (p, 7);
  /* Verify that equality is not diagnosed at the default level
     (it is diagnosed at level 3).  */
  *moved = !(p == q);
  return q;
}

void* nowarn_realloc_unequal (void *p, int *moved)
{
  void *q = realloc (p, 7);
  /* Verify that inequality is not diagnosed at the default level
     (it is diagnosed at level 3).  */
  *moved = p != q;
  return q;
}

void* warn_realloc_relational (void *p, int *rel)
{
  void *q = realloc (p, 7);       // { dg-message "call to '\(void\\* \)?realloc\(\\(void\\*, size_t\\)\)?'" "note" }
  /* Verify that all relational expressions are diagnosed at the default
     level.  */
  rel[0] = (char*)p < (char*)q;  // { dg-warning "-Wuse-after-free" }
  rel[1] = (char*)p <= (char*)q; // { dg-warning "-Wuse-after-free" }
  rel[2] = (char*)p >= (char*)q; // { dg-warning "-Wuse-after-free" }
  rel[3] = (char*)p > (char*)q;  // { dg-warning "-Wuse-after-free" }
  return q;
}

void* warn_realloc_unchecked (void *p, int *moved)
{
  void *q = realloc (p, 7);       // { dg-message "call to '\(void\\* \)?realloc\(\\(void\\*, size_t\\)\)?'" "note" }
  /* Use subtraction rather than inequality to trigger the warning
     at the default level (equality is diagnosed only at level 3).  */
  *moved = (char*)p - (char*)q;   // { dg-warning "-Wuse-after-free" }
  return q;
}

void* nowarn_realloc_unchecked_copy (void *p1, void *p2, const void *s,
				     int n, int *x)
{
  void *p3 = memcpy (p1, s, n);
  void *p4 = realloc (p2, 7);
  *x = p3 != p4;
  return p4;
}

void* warn_realloc_unchecked_copy (void *p, const void *s, int n, int *moved)
{
  void *p2 = memcpy (p, s, n);
  void *q = realloc (p, 7);       // { dg-message "call to '\(void\\* \)?realloc\(\\(void\\*, size_t\\)\)?'" "note" }
  *moved = (char*)p2 - (char*)q;  // { dg-warning "-Wuse-after-free" }
  return q;
}

void* warn_realloc_failed (void *p, int *moved)
{
  void *q = realloc (p, 7);           // { dg-message "call to '\(void\\* \)?realloc\(\\(void\\*, size_t\\)\)?'" "note" }
  if (q)
    {
      /* When realloc succeeds the original pointer is invalid.  */
      *moved = (char*)p - (char*)q;   // { dg-warning "-Wuse-after-free" }
      return q;
    }

  return p;
}

extern void *evp;

void* warn_realloc_extern (void *p, int *moved)
{
  evp = realloc (p, 7);
  if (evp)
    {
      /* When realloc succeeds the original pointer is invalid.  */
      *moved = (char*)p - (char*)evp;   // { dg-warning "-Wuse-after-free" "escaped" }
      return evp;
    }

  return p;                   // { dg-bogus "-Wuse-after-free" "safe use after realloc failure" { xfail *-*-* } }
}

struct A { void *p, *q; int moved; };

void* warn_realloc_arg (struct A *p)
{
  p->q = realloc (p->p, 7);
  if (p->q)
    {
      /* When realloc succeeds the original pointer is invalid.  */
      p->moved = p->p != p->q;  // { dg-warning "-Wuse-after-free" "escaped" { xfail *-*-* } }
      return p->q;
    }

  return p->p;
}
